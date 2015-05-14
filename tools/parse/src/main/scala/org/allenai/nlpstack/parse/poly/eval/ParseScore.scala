package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser.{ ArcLabel, PolytreeParse }

/** A ParseScore maps a candidate parse to a score. */
abstract class ParseScore extends (PolytreeParse => Double) {
  def apply(candParse: PolytreeParse): Double = {
    val (numerator, denominator) = getRatio(candParse)
    if (denominator == 0) {
      0.0
    } else {
      numerator.toFloat / denominator
    }
  }

  def getRatio(candidateParse: PolytreeParse): (Int, Int)

  def name: String
}

/** The PathAccuracyScore computes the percentage of a candidate parse's tokens that have a
  * completely correct breadcrumb path (i.e. if you follow a token's breadcrumbs to the nexus
  * in both the candidate and the gold parse, you encounter the same set of tokens in the same
  * order).
  */
abstract class PathAccuracyScore(
    goldParses: ParseBank,
    ignorePunctuation: Boolean,
    ignorePathLabels: Boolean,
    useCrumbOnly: Boolean
) extends ParseScore {

  /** Get the number of correct paths and the number of total paths in a candidate parse.
    *
    * @param candParse the candidate parse to evaluate
    * @return the pair (num correct paths, num total paths)
    */
  def getRatio(candParse: PolytreeParse): (Int, Int) = {
    goldParses.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        val validTokens = Range(1, goldParse.tokens.size) filter { tokIndex =>
          !ignorePunctuation ||
            goldParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }
        val numCorrect: Int = validTokens count {
          case token =>
            PathAccuracyScore.comparePaths(
              token,
              candParse, goldParse, useCrumbOnly, ignorePathLabels
            )
        }
        (numCorrect, validTokens.size)
      case None => (0, 0)
    }
  }
}

object PathAccuracyScore {

  def findEarliestPathDifference(
    token: Int,
    candidateParse: PolytreeParse,
    goldParse: PolytreeParse
  ): Option[Int] = {

    val candidatePath = candidateParse.paths(token) :+ token
    val goldPath = goldParse.paths(token) :+ token
    candidatePath.zip(goldPath) find { case (x, y) => x != y } map { z => z._2 }
  }

  def comparePaths(
    token: Int,
    candidateParse: PolytreeParse,
    goldParse: PolytreeParse,
    useCrumbOnly: Boolean,
    ignorePathLabels: Boolean
  ): Boolean = {

    /** Converts a path in the parse tree to its arc labels. */
    def convertPathToArcLabels(path: Seq[Int], parse: PolytreeParse): Seq[ArcLabel] = {
      path map { pathToken =>
        parse.breadcrumbArcLabel(pathToken)
      }
    }
    val candidatePath = candidateParse.paths(token)
    val goldPath = goldParse.paths(token)
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

}

case class UnlabeledAttachmentScore(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, true, true) {

  override val name: String = "UAS (no punc)"
}

case class LabeledAttachmentScore(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, false, true) {

  override val name: String = "LAS (no punc)"
}

case class UnlabeledLostTokens(goldParses: ParseBank)
    extends ParseScore {

  private val upa = UnlabeledPathAccuracy(goldParses)

  override def getRatio(candParse: PolytreeParse): (Int, Int) = {
    val (upaNumer, upaDenom) = upa.getRatio(candParse)
    (upaDenom - upaNumer, upaDenom)
  }

  override val name: String = "Lost tokens (unlabeled, no punc)"
}

case class LabeledLostTokens(goldParses: ParseBank)
    extends ParseScore {

  private val lpa = LabeledPathAccuracy(goldParses)

  override def getRatio(candParse: PolytreeParse): (Int, Int) = {
    val (lpaNumer, lpaDenom) = lpa.getRatio(candParse)
    (lpaDenom - lpaNumer, lpaDenom)
  }

  override val name: String = "Lost tokens (labeled, no punc)"
}

case class UnlabeledPathAccuracy(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, true, false) {

  override val name: String = "UPA (no punc)"
}

case class LabeledPathAccuracy(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, false, false) {

  override val name: String = "LPA (no punc)"
}

case class PostagAccuracy(goldParses: ParseBank)
    extends ParseScore {

  override val name = "Cpos accuracy"

  def getRatio(candParse: PolytreeParse): (Int, Int) = {
    goldParses.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        val numCorrect =
          candParse.tokens.tail.zip(goldParse.tokens.tail) count {
            case (candToken, goldToken) =>
              candToken.getDeterministicProperty('cpos) ==
                goldToken.getDeterministicProperty('cpos)
          }
        (numCorrect, candParse.tokens.tail.size)
      case None => (0, 0)
    }
  }
}
