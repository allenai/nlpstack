package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser.{ NoArcLabel, ArcLabel, PolytreeParse }

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

  /** Returns a ratio, typically correctEvents / totalEvents.
    *
    * @param candidateParse the parse to evaluate
    * @return a ratio, typically correctEvents / totalEvents
    */
  def getRatio(candidateParse: PolytreeParse): (Int, Int)

  /** A friendly name for this scoring function. */
  def name: String
}

/** The PathAccuracyScore computes the percentage of a candidate parse's tokens that have a
  * completely correct breadcrumb path (i.e. if you follow a token's breadcrumbs to the nexus
  * in both the candidate and the gold parse, you encounter the same set of tokens in the same
  * order).
  *
  * @param goldParseBank a bank containing gold parses
  * @param ignorePunctuation set to true if you do not want to count paths from punctuation tokens
  * @param ignorePathLabels set to true if you do not want to consider arclabels when determining
  * the equivalence of two paths
  * @param useCrumbOnly set to true if you only want to consider the breadcrumb when determining
  * the equivalence of two paths
  */
abstract class PathAccuracyScore(
    goldParseBank: ParseBank,
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
    goldParseBank.askForGoldParse(candParse) match {
      case Some(goldParse) =>
        val validTokens = Range(1, goldParse.tokens.size) filter { tokIndex =>
          !ignorePunctuation ||
            goldParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }
        val numCorrect: Int = validTokens count {
          case token =>
            PathAccuracyScore.findEarliestPathDifference(
              token,
              candParse, goldParse, useCrumbOnly, ignorePathLabels
            ) == None
        }
        (numCorrect, validTokens.size)
      case None => (0, 0)
    }
  }
}

object PathAccuracyScore {

  /** Given a sentence token and two parses of that sentence, find the earliest point of divergence
    * between the two parse paths for that token. For example, if the paths for token 5 are
    * (0,3,6,4,5) and (0,3,2,1,5) in the two parses, then the tuple (6, 2) would be returned
    * (i.e. the candidate token and the gold token at which the paths diverged).
    *
    * If the paths are equivalent, then None is returned.
    *
    * @param token the token of interest
    * @param candidateParse a candidate parse for the sentence
    * @param goldParse a gold parse of the sentence
    * @param ignorePathLabels ignore arc labels when determining path equivalence
    * @param useCrumbOnly only consider the breadcrumb, not the entire path
    * @return the point of path divergence (see above), if it exists
    */
  def findEarliestPathDifference(
    token: Int,
    candidateParse: PolytreeParse,
    goldParse: PolytreeParse,
    ignorePathLabels: Boolean = false,
    useCrumbOnly: Boolean = false
  ): Option[(Int, Int)] = {

    /** Converts a path in the parse tree to its arc labels. */
    def convertPathToArcLabels(path: Seq[Int], parse: PolytreeParse): Seq[ArcLabel] = {
      NoArcLabel +: (path.tail map { pathToken =>
        parse.breadcrumbArcLabel(pathToken)
      })
    }
    val candidatePath =
      (if (useCrumbOnly) {
        Seq(candidateParse.breadcrumb(token))
      } else {
        candidateParse.paths(token)
      }) :+ token
    val goldPath =
      (if (useCrumbOnly) {
        Seq(goldParse.breadcrumb(token))
      } else {
        goldParse.paths(token)
      }) :+ token
    if (ignorePathLabels) {
      candidatePath.zip(goldPath) find { case (x, y) => x != y }
    } else {
      candidatePath.zip(convertPathToArcLabels(candidatePath, candidateParse)).zip(
        goldPath.zip(convertPathToArcLabels(goldPath, goldParse))
      ) find { case (x, y) => x != y } map { case (x, y) => (x._1, y._1) }
    }
  }
}

/** Shorthand for the PathAccuracyScore ignoring path labels, punctuation, and using breadcrumbs
  * only for path equivalence.
  */
case class UnlabeledAttachmentScore(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, true, true) {

  override val name: String = "UAS (no punc)"
}

/** Shorthand for the PathAccuracyScore ignoring punctuation, and using breadcrumbs
  * only for path equivalence.
  */
case class LabeledAttachmentScore(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, false, true) {

  override val name: String = "LAS (no punc)"
}

/** Shorthand for the PathAccuracyScore ignoring path labels and punctuation.
  */
case class UnlabeledPathAccuracy(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, true, false) {

  override val name: String = "UPA (no punc)"
}

/** Shorthand for the PathAccuracyScore ignoring punctuation.
  */
case class LabeledPathAccuracy(goldParses: ParseBank)
    extends PathAccuracyScore(goldParses, true, false, false) {

  override val name: String = "LPA (no punc)"
}

/** Counts the number of tokens "lost" by bad attachments (i.e. their path to the root is different
  * in the gold and candidate parse). This version ignores arc labels for the purpose of path
  * equivalence.
  */
case class UnlabeledLostTokens(goldParses: ParseBank)
    extends ParseScore {

  private val upa = UnlabeledPathAccuracy(goldParses)

  override def getRatio(candParse: PolytreeParse): (Int, Int) = {
    val (upaNumer, upaDenom) = upa.getRatio(candParse)
    (upaDenom - upaNumer, upaDenom)
  }

  override val name: String = "Lost tokens (unlabeled, no punc)"
}

/** Counts the number of tokens "lost" by bad attachments (i.e. their path to the root is different
  * in the gold and candidate parse).
  */
case class LabeledLostTokens(goldParses: ParseBank)
    extends ParseScore {

  private val lpa = LabeledPathAccuracy(goldParses)

  override def getRatio(candParse: PolytreeParse): (Int, Int) = {
    val (lpaNumer, lpaDenom) = lpa.getRatio(candParse)
    (lpaDenom - lpaNumer, lpaDenom)
  }

  override val name: String = "Lost tokens (labeled, no punc)"
}

/** Counts the fraction of correctly labeled coarse part-of-speech tags in a candidate parse.
  *
  * @param goldParseBank the bank containing the gold parses
  */
case class PostagAccuracy(goldParseBank: ParseBank)
    extends ParseScore {

  override val name = "Cpos accuracy"

  def getRatio(candParse: PolytreeParse): (Int, Int) = {
    goldParseBank.askForGoldParse(candParse) match {
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
