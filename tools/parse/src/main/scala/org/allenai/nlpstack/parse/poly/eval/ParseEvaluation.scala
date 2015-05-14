package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser._

object ParseEvaluation {

  def scoreParseSource(scoringFunction: ParseScore, parseSource: PolytreeParseSource): (Int, Int) = {
    (parseSource.parseIterator map { candidateParse =>
      scoringFunction.getRatio(candidateParse)
    }) reduce { (x, y) => (x._1 + y._1, x._2 + y._2) }
  }

  def analyzeParseSource(analyzer: ParseAnalyzer, parseSource: PolytreeParseSource): Map[Symbol, Double] = {
    (parseSource.parseIterator map { candidateParse =>
      analyzer(candidateParse)
    }) reduce { (x, y) =>
      (x.keySet ++ y.keySet map { key =>
        (key, x.getOrElse(key, 0.0) + y.getOrElse(key, 0.0))
      }).toMap
    }
  }

  def performStandardEvaluation(
    candidateSource: PolytreeParseSource,
    goldParseBank: ParseBank
  ) {

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

  def performStandardAnalysis(parseSource: PolytreeParseSource, goldParseBank: ParseBank): Unit = {
    val analyzers: Seq[ParseAnalyzer] = Seq(
      MisattachmentAnalyzer(goldParseBank, ignorePathLabels = true),
      LostTokensAnalyzer(goldParseBank),
      CposErrorAnalyzer(goldParseBank)
    )
    val allAnalyses = for {
      analyzer <- analyzers
    } yield {
      (analyzer.name, ParseEvaluation.analyzeParseSource(analyzer, parseSource))
    }
    allAnalyses foreach {
      case (analyzerName, analysis) =>
        println(s"ARCLABEL\t$analyzerName")
        analysis foreach {
          case (label, count) =>
            println(s"${label.name.toLowerCase}\t$count")
        }
    }
  }

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
