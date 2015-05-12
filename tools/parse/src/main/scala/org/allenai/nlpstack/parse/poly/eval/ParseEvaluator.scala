package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser.{ ArcLabel, InMemoryPolytreeParseSource, PolytreeParseSource, PolytreeParse }

import java.io._

object ParseEvaluator {

  /** Collects statistics over a sequence of (candidate parse, gold parse) pairs.
    *
    * Essentially this simply iterates through the candidate and gold parses. For each parse
    * pair, it notifies the desired ParseStatistics.
    *
    * @param candidateParses an Iterator over candidate parses (can be None if a parse failure
    * occurred)
    * @param goldParses an Iterator over gold parses
    * @param statistics a set of ParseStatistic objects (they collect the statistics via their
    * .notify() method)
    */
  def evaluate(
    candidateParses: Iterator[Option[PolytreeParse]],
    goldParses: Iterator[PolytreeParse],
    statistics: Seq[ParseStatistic],
    diagnosticWriter: Option[PrintWriter] = None
  ) {

    for {
      (candidateParse, goldParse) <- candidateParses.zip(goldParses)
      stat <- statistics
    } stat.notify(candidateParse, goldParse)
    for {
      stat <- statistics
    } stat.report(diagnosticWriter)
  }

  def compareParseBanks(
    bank1: Map[String, PolytreeParse],
    bank2: Map[String, PolytreeParse],
    statistics: Seq[ParseStatistic]
  ): Unit = {

    val commonKeys = bank1.keySet intersect bank2.keySet
    compareParseStreamToParseBank(
      bank1.toSeq filter { case (key, value) => commonKeys.contains(key) },
      bank2,
      statistics
    )
  }

  def compareParseStreamToParseBank(
    parses: Seq[(String, PolytreeParse)],
    bank: Map[String, PolytreeParse],
    statistics: Seq[ParseStatistic]
  ): Unit = {

    for {
      stat <- statistics
    } stat.reset()
    val bankKeys = bank.keySet
    for {
      (sentence, parse1) <- parses
      stat <- statistics
      if bankKeys.contains(sentence)
    } {
      val parse2 = bank(sentence)
      stat.notify(Some(parse1), parse2)
    }
  }
}

/** A ParseStatistic accrues a particular statistic over (candidate parse, gold parse) pairs.
  * Every time .notify() is called, the statistic is updated.
  */
abstract class ParseStatistic {
  def reset(): Unit = {}

  def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit

  def result(): Double

  /** Display a report about the accumulated statistics to stdout. */
  def report(statWriter: Option[PrintWriter]): Unit

  def logMessage(statWriter: Option[PrintWriter], message: String): Unit = {
    statWriter match {
      case Some(w) => w.println(message)
      case None => println(message)
    }
  }
}

/** CposAccuracy stores the statistics necessary to compute coarse-part-of-speech tagging
  * accuracy, which is the percentage of tokens assigned the correct CPOS by the parser.
  *
  * @param verbose if true, reporting gives more detailed error analysis of the tagging mistakes
  */
case class CposAccuracy(verbose: Boolean = false) extends ParseStatistic {
  var numCorrect = 0
  var numTotal = 0
  var numParses = 0
  var errorHistogram = Map[(Symbol, Symbol), Int]()

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    candidateParse match {
      case Some(candParse) =>
        if (candParse.breadcrumb.size == goldParse.breadcrumb.size) {
          numParses += 1
          val totalIncrement = goldParse.breadcrumb.tail.size
          numTotal += totalIncrement
          // skip the first element because it is the nexus (hence it has no part-of-speech)
          val correctIncrement =
            candParse.tokens.tail.zip(goldParse.tokens.tail) count {
              case (candToken, goldToken) =>
                candToken.getDeterministicProperty('cpos) ==
                  goldToken.getDeterministicProperty('cpos)
            }
          numCorrect += correctIncrement
          candParse.tokens.tail.zip(goldParse.tokens.tail) filter {
            case (candToken, goldToken) =>
              candToken.getDeterministicProperty('cpos) != goldToken.getDeterministicProperty('cpos)
          } foreach {
            case (candToken, goldToken) =>
              val candCpos = candToken.getDeterministicProperty('cpos)
              val goldCpos = goldToken.getDeterministicProperty('cpos)
              errorHistogram = errorHistogram.updated(
                (candCpos, goldCpos),
                1 + errorHistogram.getOrElse((candCpos, goldCpos), 0)
              )
          }
        } else { // skip the parse if the tokenization is different
          println(s"WARNING -- Skipping parse: ${candParse.sentence.asWhitespaceSeparatedString}" +
            s" tokenized differently than gold: ${goldParse.sentence.asWhitespaceSeparatedString}")
        }
      case None =>
        numParses += 1
        numTotal += goldParse.breadcrumb.tail.size
        println(s"WARNING -- Failed parse")
    }
  }

  override def result(): Double = {
    (100.0 * numCorrect) / numTotal
  }

  override def report(statWriter: Option[PrintWriter]): Unit = {
    println("Cpos Tagging: %d / %d = %2.2f%%".format(numCorrect, numTotal,
      result()))
    if (verbose) {
      errorHistogram.toSeq sortBy {
        case (_, count) =>
          count
      } foreach {
        case ((candCpos, goldCpos), count) =>
          logMessage(statWriter, s"  $count: ${goldCpos.name} ~~> ${candCpos.name}")
      }
    }
  }

  override def reset(): Unit = {
    numCorrect = 0
    numTotal = 0
    numParses = 0
  }
}

/** A ParseScore maps a candidate parse to a score. */
trait ParseScore extends (PolytreeParse => Double) {
  def getRatio(candidateParse: PolytreeParse): (Int, Int)
}

case class PathAccuracy(ignorePunctuation: Boolean, ignorePathLabels: Boolean, useCrumbOnly: Boolean = false)
    extends ParseStatistic {

  var numCorrect = 0
  var numTotal = 0
  var numParses = 0
  var accumulatedImpact = Map[Symbol, Int]()
  var errorHistogram = Map[Symbol, Int]()
  var labelHistogram = Map[Symbol, Int]()

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    numParses += 1
    val scoringFunction = PathAccuracyScore(
      InMemoryPolytreeParseSource(Seq(goldParse)),
      ignorePunctuation, ignorePathLabels, useCrumbOnly
    )
    candidateParse map { parse =>
      scoringFunction.getRatio(parse)
    } foreach {
      case (correctIncrement, totalIncrement) =>
        numCorrect += correctIncrement
        numTotal += totalIncrement
      //println(s"Sentence: ${goldParse.sentence.asWhitespaceSeparatedString}")
      //println(s"  ${correctIncrement / totalIncrement.toFloat}")
    }

    candidateParse map { parse =>
      scoringFunction.getErrorAnalysis(parse)
    } foreach { hist =>
      errorHistogram = (errorHistogram.keySet ++ hist.keySet map { key =>
        (key, errorHistogram.getOrElse(key, 0) + hist.getOrElse(key, 0))
      }).toMap
    }
    candidateParse map { parse =>
      scoringFunction.getImpactAnalysis(parse)
    } foreach { hist =>
      accumulatedImpact = (accumulatedImpact.keySet ++ hist.keySet map { key =>
        (key, accumulatedImpact.getOrElse(key, 0) + hist.getOrElse(key, 0))
      }).toMap
    }
    val newLabelHistogram: Map[Symbol, Int] = goldParse.breadcrumbArcLabel.tail map { arcLabel =>
      arcLabel.toSymbol
    } groupBy { x => x } mapValues { x => x.size }
    labelHistogram = (labelHistogram.keySet ++ newLabelHistogram.keySet map { key =>
      (key, labelHistogram.getOrElse(key, 0) + newLabelHistogram.getOrElse(key, 0))
    }).toMap
  }

  override def result(): Double = {
    (100.0 * numCorrect) / numTotal
  }

  override def report(statWriter: Option[PrintWriter]): Unit = {
    val puncNote = Map(true -> "ignorePunc", false -> "full")
    val metricName: String =
      (if (ignorePathLabels) { "U" } else { "L" }) + (if (useCrumbOnly) { "AS" } else { "PA" })
    println(s"$metricName (${puncNote(ignorePunctuation)}): " +
      s"%d / %d = %2.2f%%".format(numCorrect, numTotal,
        result()))
    println("Errors:")
    errorHistogram.toSeq sortBy { _._2 } foreach {
      case (error, count) =>
        println(s"  ${error.name}: $count / ${labelHistogram.getOrElse(error, 0)} = ${count * 1.0 / labelHistogram.getOrElse(error, 0)}")
    }
    println("Impact:")
    accumulatedImpact.toSeq sortBy { _._2 } foreach {
      case (error, count) =>
        println(s"  ${error.name}: $count")
    }
  }

  override def reset(): Unit = {
    numCorrect = 0
    numTotal = 0
    numParses = 0
    accumulatedImpact = Map[Symbol, Int]()
    errorHistogram = Map[Symbol, Int]()
    labelHistogram = Map[Symbol, Int]()
  }
}

object UnlabeledAttachmentScore extends PathAccuracy(true, true, true)
object LabeledAttachmentScore extends PathAccuracy(true, false, true)
object UnlabeledPathAccuracy extends PathAccuracy(true, true, false)
object LabeledPathAccuracy extends PathAccuracy(true, false, false)

/** The PathAccuracyScore computes the percentage of a candidate parse's tokens that have a
  * completely correct breadcrumb path (i.e. if you follow a token's breadcrumbs to the nexus
  * in both the candidate and the gold parse, you encounter the same set of tokens in the same
  * order).
  */
case class PathAccuracyScore(
    goldParseSource: PolytreeParseSource,
    ignorePunctuation: Boolean, ignorePathLabels: Boolean, useCrumbOnly: Boolean
) extends ParseScore {

  private val goldParses: Map[String, PolytreeParse] = (goldParseSource.parseIterator map { parse =>
    (parse.sentence.asWhitespaceSeparatedString, parse)
  }).toMap

  final def apply(candParse: PolytreeParse): Double = {
    val (numerator, denominator) = getRatio(candParse)
    if (denominator == 0) {
      0.0
    } else {
      numerator.toFloat / denominator
    }
  }

  /** Get the number of correct paths and the number of total paths in a candidate parse.
    *
    * @param candParse the candidate parse to evaluate
    * @return the pair (num correct paths, num total paths)
    */
  def getRatio(candParse: PolytreeParse): (Int, Int) = {
    getGoldParse(candParse) match {
      case Some(goldParse) =>
        val goldAndCandidatePaths = pairGoldAndCandidatePaths(candParse, goldParse)
        val numCorrect: Int = goldAndCandidatePaths count {
          case (token, goldPath, candidatePath) =>
            comparePaths(token, candidatePath, goldPath, candParse, goldParse)
        }
        (numCorrect, goldAndCandidatePaths.size)
      case None => (0, 0)
    }
  }

  def getErrorAnalysis(candParse: PolytreeParse): Map[Symbol, Int] = {
    getGoldParse(candParse) match {
      case Some(goldParse) =>
        val goldAndCandidatePaths = pairGoldAndCandidatePaths(candParse, goldParse)
        val incorrectTokens: Iterable[Int] = goldAndCandidatePaths filter {
          case (token, goldPath, candidatePath) =>
            !comparePaths(token, candidatePath, goldPath, candParse, goldParse)
        } map {
          case (token, goldPath, candidatePath) =>
            token
        }
        val result = (incorrectTokens map { tokIndex =>
          goldParse.breadcrumbArcLabel(tokIndex).toSymbol
        }) groupBy { x => x } mapValues { y => y.size }
        result
      case None => Map[Symbol, Int]()
    }
  }

  def getImpactAnalysis(candParse: PolytreeParse): Map[Symbol, Int] = {
    getGoldParse(candParse) match {
      case Some(goldParse) =>
        val goldAndCandidatePaths = pairGoldAndCandidatePaths(candParse, goldParse)
        val goldAndCandidatePathMap = (goldAndCandidatePaths map {
          case (tokIndex, goldPath, candPath) =>
            (tokIndex, (goldPath, candPath))
        }).toMap
        val incorrectTokens: Iterable[Int] = goldAndCandidatePaths filter {
          case (token, goldPath, candidatePath) =>
            goldPath != candidatePath
          //!comparePaths(token, candidatePath, goldPath, candParse, goldParse)
        } map {
          case (token, goldPath, candidatePath) =>
            token
        }
        val lostTokens: Set[Int] = (incorrectTokens.toSet flatMap { tokIndex: Int =>
          goldParse.getGretels(tokIndex)
        }) ++ incorrectTokens.toSet filter { tokIndex: Int =>
          goldAndCandidatePathMap.contains(tokIndex)
        }
        val blameTokens: Seq[Int] = lostTokens.toSeq map { lostToken =>
          //println(goldAndCandidatePathMap)
          val goldAndCandidatePaths = goldAndCandidatePathMap(lostToken)
          findEarliestPathDifference(goldAndCandidatePaths._2 :+ lostToken, goldAndCandidatePaths._1 :+ lostToken).get
        }
        val result = (blameTokens map { tokIndex =>
          goldParse.breadcrumbArcLabel(tokIndex).toSymbol
        }) groupBy { x => x } mapValues { y => y.size }
        result

      //val result = (incorrectTokens map { tokIndex =>
      //  (goldParse.breadcrumbArcLabel(tokIndex).toSymbol, 1 + goldParse.getGretels(tokIndex).size)
      //}) groupBy { x => x._1 } mapValues { y => (y map { _._2}).sum}
      //result
      case None => Map[Symbol, Int]()
    }
  }

  private def getGoldParse(candParse: PolytreeParse): Option[PolytreeParse] = {
    goldParses.get(candParse.sentence.asWhitespaceSeparatedString) match {
      case Some(goldParse) =>
        if (candParse.breadcrumb.size != goldParse.breadcrumb.size ||
          goldParse.breadcrumb.size <= 1) {

          println(s"WARNING -- Skipping parse: ${candParse.sentence.asWhitespaceSeparatedString}" +
            s" tokenized differently than gold: ${goldParse.sentence.asWhitespaceSeparatedString}")
          None
        } else {
          Some(goldParse)
        }
      case None => None
    }
  }

  private def pairGoldAndCandidatePaths(
    candParse: PolytreeParse,
    goldParse: PolytreeParse
  ): Iterable[(Int, Seq[Int], Seq[Int])] = {

    val zippedPaths = {
      val zipped = candParse.paths.zipWithIndex.tail.zip(
        goldParse.paths.tail
      )
      if (ignorePunctuation) {
        zipped filter {
          case ((x, i), y) =>
            goldParse.tokens(i).getDeterministicProperty('cpos) != Symbol(".")
        }
      } else {
        zipped
      }
    }
    (zippedPaths map {
      case ((candidatePath, token), goldPath) =>
        (token, goldPath, candidatePath)
    }).toIterable
  }

  private def comparePaths(token: Int, candidatePath: Seq[Int], goldPath: Seq[Int],
    candidateParse: PolytreeParse, goldParse: PolytreeParse): Boolean = {
    if (useCrumbOnly) {
      (candidatePath.last == goldPath.last) &&
        (ignorePathLabels ||
          candidateParse.breadcrumbArcLabel(token) ==
          goldParse.breadcrumbArcLabel(token))
    } else {
      (candidatePath == goldPath) &&
        (ignorePathLabels ||
          convertPathToArcLabels(candidatePath :+ token, candidateParse) ==
          convertPathToArcLabels(goldPath :+ token, goldParse))
    }
  }

  private def findEarliestPathDifference(candidatePath: Seq[Int], goldPath: Seq[Int]): Option[Int] = {
    candidatePath.zip(goldPath) find { case (x, y) => x != y } map { z => z._2 }
  }

  /** Converts a path in the parse tree to its arc labels. */
  private def convertPathToArcLabels(path: Seq[Int], parse: PolytreeParse): Seq[ArcLabel] = {
    path map { pathToken =>
      parse.breadcrumbArcLabel(pathToken)
    }
  }
}
