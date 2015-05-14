package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse

/** A ParseAnalyzer maps a candidate parse to an analysis. */
trait ParseAnalyzer extends (PolytreeParse => Map[Symbol, Double]) {
  def name: String
}

case class LostTokensAnalyzer(goldParseBank: ParseBank) extends ParseAnalyzer {

  def apply(candParse: PolytreeParse): Map[Symbol, Double] = {
    goldParseBank.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        val validTokens = Range(1, goldParse.tokens.size) filter { tokIndex =>
          goldParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }
        val lostTokens = validTokens filter {
          case token =>
            !PathAccuracyScore.comparePaths(
              token,
              candParse, goldParse, useCrumbOnly = false, ignorePathLabels = true
            )
        }
        val blameTokens: Seq[Int] = lostTokens.toSeq map { lostToken =>
          PathAccuracyScore.findEarliestPathDifference(lostToken, candParse, goldParse).get
        }
        (blameTokens map { tokIndex =>
          goldParse.breadcrumbArcLabel(tokIndex).toSymbol
        }) groupBy { x => x } mapValues { y => y.size.toDouble }
      case None =>
        Map[Symbol, Double]()
    }
  }

  override val name: String = "TOKENS LOST"
}

case class MisattachmentAnalyzer(
    goldParseBank: ParseBank,
    ignorePathLabels: Boolean
) extends ParseAnalyzer {

  def apply(candParse: PolytreeParse): Map[Symbol, Double] = {
    goldParseBank.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        val validTokens = Range(1, goldParse.tokens.size) filter { tokIndex =>
          goldParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }
        val incorrectTokens = validTokens filter {
          case token =>
            !PathAccuracyScore.comparePaths(
              token,
              candParse, goldParse, useCrumbOnly = true, ignorePathLabels
            )
        }
        (incorrectTokens map { tokIndex =>
          goldParse.breadcrumbArcLabel(tokIndex).toSymbol
        }) groupBy { x => x } mapValues { y => y.size.toDouble }
      case None => Map[Symbol, Double]()
    }
  }

  override val name: String = "MISATTACHMENT FREQUENCY"
}

case class CposErrorAnalyzer(goldParseBank: ParseBank) extends ParseAnalyzer {

  override val name: String = "CPOS ERROR FREQUENCY"

  def apply(candParse: PolytreeParse): Map[Symbol, Double] = {
    goldParseBank.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        var errorHistogram = Map[Symbol, Double]()
        candParse.tokens.tail.zip(goldParse.tokens.tail) filter {
          case (candToken, goldToken) =>
            candToken.getDeterministicProperty('cpos) != goldToken.getDeterministicProperty('cpos)
        } foreach {
          case (candToken, goldToken) =>
            val candCpos = candToken.getDeterministicProperty('cpos)
            val goldCpos = goldToken.getDeterministicProperty('cpos)
            val histogramKey = stringifyError(candCpos, goldCpos)
            errorHistogram = errorHistogram.updated(
              histogramKey,
              1 + errorHistogram.getOrElse(histogramKey, 0.0)
            )
        }
        errorHistogram
      case None => Map[Symbol, Double]()
    }
  }

  private def stringifyError(candidateTag: Symbol, goldTag: Symbol): Symbol = {
    Symbol(s"${goldTag.name}-->${candidateTag.name}")
  }
}
