package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse

/** A ParseAnalyzer maps a candidate parse to an "analysis", i.e. a histogram.
  *
  * For instance, this histogram may be the count of mistaken arclabels or part-of-speech tags.
  */
trait ParseAnalyzer extends (PolytreeParse => Map[String, Double]) {
  def name: String
}

/** The MisattachmentAnalyzer tallies misattached tokens (i.e. tokens with the wrong breadcrumb
  * assignment) according to the label of its breadcrumb arc in the gold parse.
  *
  * @param goldParseBank a bank containing the gold parses
  * @param ignoreLabel set to true if we want to regard a node as correctly attached as long as
  * its breadcrumb is correct (regardless of how its breadcrumb arc is labeled)
  */
case class MisattachmentAnalyzer(
    goldParseBank: ParseBank,
    ignoreLabel: Boolean
) extends ParseAnalyzer {

  def apply(candParse: PolytreeParse): Map[String, Double] = {
    goldParseBank.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        val validTokens = Range(1, goldParse.tokens.size) filter { tokIndex =>
          !goldParse.tokens(tokIndex).isPunctuation
        }
        val incorrectTokens = validTokens filter {
          case token =>
            PathAccuracyScore.findEarliestPathDifference(
              token,
              candParse, goldParse, ignorePathLabels = ignoreLabel, useCrumbOnly = true
            ) != None
        }
        (incorrectTokens map { tokIndex =>
          goldParse.breadcrumbArcLabel(tokIndex).toString
        }) groupBy { x => x } mapValues { y => y.size.toDouble }
      case None => Map[String, Double]()
    }
  }

  override val name: String = "MISATTACHMENT FREQUENCY"
}

/** The LostTokensAnalyzer tallies lost tokens (i.e. tokens with a different breadcrumb path in the
  * gold parse) according to the breadcrumb arc label of their highest misattached ancestor in the
  * gold parse.
  *
  * Example: In the gold parse, suppose the breadcrumb path of token "red" is
  *
  * --ROOT--> ate --PREP--> with --POBJ--> chopsticks --AMOD--> red
  *
  * but in the candidate parse, the breadcrumb path of token "chopsticks" is
  *
  * --ROOT--> ate --DOBJ--> pasta --PREP--> with --POBJ--> meatballs --AMOD--> red
  *
  * then the highest misattached ancestor of "red" in the gold parse is "with" (attached to "pasta"
  * instead of "ate"). The arc label of "with" is "PREP" in the gold parse. So the loss of token
  * "red" is attributed to a "PREP" attachment error.
  *
  * @param goldParseBank a bank containing the gold parses
  */
case class LostTokensAnalyzer(goldParseBank: ParseBank) extends ParseAnalyzer {

  def apply(candParse: PolytreeParse): Map[String, Double] = {
    goldParseBank.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        val validTokens = Range(1, goldParse.tokens.size) filter { tokIndex =>
          !goldParse.tokens(tokIndex).isPunctuation
        }
        val lostTokens = validTokens filter {
          case token =>
            PathAccuracyScore.findEarliestPathDifference(
              token,
              candParse, goldParse, useCrumbOnly = false, ignorePathLabels = true
            ) != None
        }
        val blameTokens: Seq[Int] = lostTokens.toSeq map { lostToken =>
          PathAccuracyScore.findEarliestPathDifference(lostToken, candParse, goldParse).get._2
        }
        (blameTokens map { tokIndex =>
          goldParse.breadcrumbArcLabel(tokIndex).toString
        }) groupBy { x => x } mapValues { y => y.size.toDouble }
      case None =>
        Map[String, Double]()
    }
  }

  override val name: String = "TOKENS LOST"
}

/** The CposErrorAnalyzer tallies coarse part-of-speech tagging errors according to the
  * specific error (i.e. "NOUN-->VERB" means that a NOUN was incorrectly tagged as a verb).
  *
  * @param goldParseBank a bank containing the gold parses
  */
case class CposErrorAnalyzer(goldParseBank: ParseBank) extends ParseAnalyzer {

  override val name: String = "CPOS ERROR FREQUENCY"

  def apply(candParse: PolytreeParse): Map[String, Double] = {
    goldParseBank.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        var errorHistogram = Map[String, Double]()
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
      case None => Map[String, Double]()
    }
  }

  private def stringifyError(candidateTag: Symbol, goldTag: Symbol): String = {
    s"${goldTag.name}-->${candidateTag.name}"
  }
}
