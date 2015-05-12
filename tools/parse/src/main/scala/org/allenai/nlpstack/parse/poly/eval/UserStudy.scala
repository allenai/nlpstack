package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

private case class UserStudyCommandLine(userFilenames: String = "", goldFilename: String = "")

object UserStudy {

  /** Command-line for evaluating a set of parses against a gold set.
    *
    * Usage: Evaluate [options]
    *
    * -c <file> | --candidate <file>
    * the file containing the candidate parses (CoNLL-X format)
    * -g <file> | --gold <file>
    * the file containing the gold parses (CoNLL-X format)
    *
    * @param args see above
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[UserStudyCommandLine]("Evaluate") {
      opt[String]('u', "user") required () valueName ("<file>") action { (x, c) => c.copy(userFilenames = x) } text ("the file containing the candidate " +
        "parses (CoNLL-X format)")
      opt[String]('g', "gold") required () valueName ("<file>") action { (x, c) => c.copy(goldFilename = x) } text ("the file containing the gold " +
        "parses (CoNLL-X format)")
    }
    val clArgs: UserStudyCommandLine = optionParser.parse(args, UserStudyCommandLine()).get

    //val userParses: Seq[Map[String, PolytreeParse]] =
    //  clArgs.userFilenames.split(",").toSeq map { userFilename =>
    //    createParseBankFromFile(userFilename)
    //  }
    val userParses = loadUserMap()
    //val mergedUserParses = mergeParseBanks(userParses)
    val goldParses: Map[String, PolytreeParse] = createParseBankFromFile(clArgs.goldFilename)

    val stats: Seq[ParseStatistic] = Seq(
      PathAccuracy(true, false, true),
      PathAccuracy(true, true, true),
      PathAccuracy(true, false, false),
      PathAccuracy(true, true, false)
    //("CPOS", CposAccuracy(false))
    )

    val userScores: Map[String, Seq[Double]] = compareUsersToGold(userParses, goldParses, stats)
    println(Seq("USER", "LAS", "UAS", "LPA", "UPA").mkString("\t"))
    userScores foreach {
      case (userName, scores) =>
        println((userName +: scores).mkString("\t"))
    }
    val interUserScores: Map[String, Seq[Double]] = compareUsersToEachOther(userParses, stats)
    println(Seq("USER", "LAS", "UAS", "LPA", "UPA").mkString("\t"))
    interUserScores foreach {
      case (userName, scores) =>
        println((userName +: scores).mkString("\t"))
    }

    val mergedUsers = mergeParseBanks(userParses.values.toSeq)
    val userErrorAnalysis =
      getParseErrorAnalysis(mergedUsers map { _._2 }, InMemoryPolytreeParseSource(goldParses.values))
    val stanBank =
      createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/barrons.devandtest.stan.conllx")
    val stanErrorAnalysis =
      getParseErrorAnalysis(stanBank.values.toSeq, InMemoryPolytreeParseSource(goldParses.values))
    printErrorAnalysisAsTSV(userErrorAnalysis)
    printErrorAnalysisAsTSV(stanErrorAnalysis)

    val userErrorImpact =
      getParseErrorImpact(mergedUsers map { _._2 }, InMemoryPolytreeParseSource(goldParses.values))
    val stanErrorImpact =
      getParseErrorImpact(stanBank.values.toSeq, InMemoryPolytreeParseSource(goldParses.values))
    printErrorAnalysisAsTSV(userErrorImpact)
    printErrorAnalysisAsTSV(stanErrorImpact)

  }

  def printErrorAnalysisAsTSV(errorAnalysis: Map[Symbol, Double]): Unit = {
    println(s"ERROR\tFREQUENCY")
    val sortedErrorAnalysis = (errorAnalysis.toSeq sortBy { x => x._2 }).reverse
    sortedErrorAnalysis foreach {
      case (key, value) =>
        println(s"${key.name.toLowerCase}\t$value")
    }
  }

  def getParseErrorAnalysis(
    candidateParses: Seq[PolytreeParse],
    goldParseSource: PolytreeParseSource
  ): Map[Symbol, Double] = {

    println(s"num parses=${candidateParses.size}")

    val scoringFunction = PathAccuracyScore(
      goldParseSource,
      ignorePunctuation = true, ignorePathLabels = true, useCrumbOnly = true
    )

    var errorHistogram = Map[Symbol, Int]()
    candidateParses map { parse =>
      scoringFunction.getErrorAnalysis(parse)
    } foreach { hist =>
      errorHistogram = (errorHistogram.keySet ++ hist.keySet map { key =>
        (key, errorHistogram.getOrElse(key, 0) + hist.getOrElse(key, 0))
      }).toMap
    }

    val tokenCount =
      PolytreeParseSource.countTokens(
        InMemoryPolytreeParseSource(candidateParses),
        excludePunctuation = true
      )
    errorHistogram mapValues { errorCount => errorCount.toDouble * 100 / tokenCount }
  }

  def getParseErrorImpact(
    candidateParses: Seq[PolytreeParse],
    goldParseSource: PolytreeParseSource
  ): Map[Symbol, Double] = {

    println(s"num parses=${candidateParses.size}")

    val scoringFunction = PathAccuracyScore(
      goldParseSource,
      ignorePunctuation = true, ignorePathLabels = true, useCrumbOnly = true
    )

    var errorHistogram = Map[Symbol, Int]()
    candidateParses map { parse =>
      scoringFunction.getImpactAnalysis(parse)
    } foreach { hist =>
      errorHistogram = (errorHistogram.keySet ++ hist.keySet map { key =>
        (key, errorHistogram.getOrElse(key, 0) + hist.getOrElse(key, 0))
      }).toMap
    }

    val tokenCount =
      PolytreeParseSource.countTokens(
        InMemoryPolytreeParseSource(candidateParses),
        excludePunctuation = true
      )
    errorHistogram mapValues { errorCount => errorCount.toDouble * 100 / tokenCount }
  }

  def compareUsersToGold(
    userParses: Map[String, Map[String, PolytreeParse]],
    goldParses: Map[String, PolytreeParse],
    stats: Seq[ParseStatistic]
  ): Map[String, Seq[Double]] = {

    for {
      (userIName, userIParses) <- userParses
    } yield {
      ParseEvaluator.compareParseBanks(userIParses, goldParses, stats)
      (userIName, stats map { stat => stat.result() })
    }
  }

  def compareUsersToEachOther(
    userParses: Map[String, Map[String, PolytreeParse]],
    stats: Seq[ParseStatistic]
  ): Map[String, Seq[Double]] = {

    for {
      (userIName, userIParses) <- userParses
    } yield {
      val otherUserParses: Map[String, PolytreeParse] = (for {
        (userJName, userJParses) <- userParses if userJName != userIName && userJName != "stan"
      } yield {
        userJParses
      }) reduce { (map1, map2) => map1 ++ map2 }

      ParseEvaluator.compareParseBanks(userIParses, otherUserParses, stats)
      (userIName, stats map { stat => stat.result() })
    }
  }

  def loadUserMap(): Map[String, Map[String, PolytreeParse]] = {
    Seq(
      ("userA", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userA.rik.conllx")),
      ("userB", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userB.tony.conllx")),
      ("userC", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userC.sumithra.conllx")),
      ("userD", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userD.sam.conllx")),
      ("userE", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userE.oyvind.conllx")),
      ("userF", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userF.kevin.conllx")),
      ("userG", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userG.cristian.conllx")),
      ("userH", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userH.chris.conllx")),
      ("userI", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userI.jesse.conllx"))
    //("stan", createParseBankFromFile("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/barrons.devandtest.stan.conllx"))
    ).toMap
  }

  def matrixPrint(matrix: Array[Array[Double]]) = {
    for {
      i <- Range(0, matrix.size)
    } println(matrix(i).toSeq.mkString(" "))
  }

  def createParseBankFromFile(
    filename: String,
    fileFormat: PolytreeParseFileFormat = ConllX(true)
  ): Map[String, PolytreeParse] = {

    val parseStream: Iterator[PolytreeParse] = PolytreeParse.fromFile(filename, fileFormat)
    (parseStream map { parse =>
      (parse.sentence.asWhitespaceSeparatedString, parse)
    }).toMap
  }

  def mergeParseBanks(banks: Seq[Map[String, PolytreeParse]]): Seq[(String, PolytreeParse)] = {
    (for {
      userJParses <- banks
    } yield {
      userJParses.toSeq
    }) reduce { (map1, map2) => map1.toSeq ++ map2.toSeq }
  }
}

