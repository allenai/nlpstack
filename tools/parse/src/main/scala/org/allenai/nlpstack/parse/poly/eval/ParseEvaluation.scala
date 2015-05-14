package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser._

object ParseEvaluation {

  /** Runs a specified scoring function over all parses in the parse source, and sums the results.
    *
    * Each scoring function returns a ratio (i.e. numerator/denominator) for each parse in
    * the parse source. The sum of these ratios is returned.
    *
    * @param scoringFunction the scoring function to apply to the parses
    * @param parseSource the source of parses
    * @return the sum of the ratios produced by calling the scoring function on each parse of the
    * parse source
    */
  def scoreParseSource(
    scoringFunction: ParseScore,
    parseSource: PolytreeParseSource
  ): (Int, Int) = {

    (parseSource.parseIterator map { candidateParse =>
      scoringFunction.getRatio(candidateParse)
    }) reduce { (x, y) => (x._1 + y._1, x._2 + y._2) }
  }

  /** Runs a specified parse analyzer over all parses in the parse source, and combines the results.
    *
    * For each parse, the analyzer returns a histogram (specifically a map from symbols to counts).
    * These maps are combined by adding the counts for each key.
    *
    * @param analyzer the analyzer to apply to the parses
    * @param parseSource the source of parses
    * @return a merging of the histograms produced by calling the analyzer on each parse of the
    * parse source
    */
  def analyzeParseSource(
    analyzer: ParseAnalyzer,
    parseSource: PolytreeParseSource
  ): Map[String, Double] = {

    (parseSource.parseIterator map { candidateParse =>
      analyzer(candidateParse)
    }) reduce { (x, y) =>
      (x.keySet ++ y.keySet map { key =>
        (key, x.getOrElse(key, 0.0) + y.getOrElse(key, 0.0))
      }).toMap
    }
  }

  /** Runs a standard set of scoring functions on a set of candidate parses with respect to a
    * bank of gold parses.
    *
    * @param candidateSource the source of candidate parses
    * @param goldParseBank a bank containing the gold parses
    */
  def performStandardEvaluation(
    candidateSource: PolytreeParseSource,
    goldParseBank: ParseBank
  ): Unit = {

    val goldScoringFunctions: Seq[ParseScore] = Seq(
      UnlabeledAttachmentScore(goldParseBank),
      LabeledAttachmentScore(goldParseBank),
      UnlabeledLostTokens(goldParseBank),
      LabeledLostTokens(goldParseBank),
      PostagAccuracy(goldParseBank)
    )
    val allScores = for {
      scoringFunction <- goldScoringFunctions
    } yield {
      (scoringFunction.name, ParseEvaluation.scoreParseSource(scoringFunction, candidateSource))
    }
    allScores foreach {
      case (scoreName, (userNumer, userDenom)) =>
        println(f"$scoreName: ${100 * userNumer.toDouble / userDenom}%.2f" + "%")
    }
  }

  /** Runs a standard set of analyzers on a set of candidate parses with respect to a
    * bank of gold parses.
    *
    * @param candidateSource the source of candidate parses
    * @param goldParseBank a bank containing the gold parses
    */
  def performStandardAnalysis(
    candidateSource: PolytreeParseSource,
    goldParseBank: ParseBank
  ): Unit = {

    val analyzers: Seq[ParseAnalyzer] = Seq(
      MisattachmentAnalyzer(goldParseBank, ignoreLabel = true),
      LostTokensAnalyzer(goldParseBank),
      CposErrorAnalyzer(goldParseBank)
    )
    val allAnalyses = for {
      analyzer <- analyzers
    } yield {
      (analyzer.name, ParseEvaluation.analyzeParseSource(analyzer, candidateSource))
    }
    allAnalyses foreach {
      case (analyzerName, analysis) =>
        println(s"ARCLABEL\t$analyzerName")
        analysis foreach {
          case (label, count) =>
            println(s"${label.toLowerCase}\t$count")
        }
    }
  }

  /** Runs a standard contrastive evaluation of a set of candidate parsers ("users")
    * with respect to a bank of gold parses.
    *
    * @param candidateSources the map from user names to candidate parses
    * @param goldParseBank a bank containing the gold parses
    */
  def performStandardMultiUserEvaluation(
    candidateSources: Map[String, PolytreeParseSource],
    goldParseBank: ParseBank
  ): Unit = {

    val goldScoringFunctions: Seq[ParseScore] = Seq(
      UnlabeledAttachmentScore(goldParseBank),
      LabeledAttachmentScore(goldParseBank),
      UnlabeledLostTokens(goldParseBank),
      LabeledLostTokens(goldParseBank),
      PostagAccuracy(goldParseBank)
    )
    val allScores = for {
      scoringFunction <- goldScoringFunctions
    } yield {
      (scoringFunction.name, for {
        (userIName, userIParses) <- candidateSources.toSeq
      } yield {
        val score = ParseEvaluation.scoreParseSource(scoringFunction, userIParses)
        (userIName, score)
      })
    }
    allScores foreach {
      case (scoreName, userScores) =>
        println(s"USER\t$scoreName")
        userScores foreach {
          case (username, (numer, denom)) =>
            println(f"$username\t${numer.toDouble / denom}%.3f")
        }
    }
  }
}
