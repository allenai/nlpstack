package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.ml.WrapperClassifier
import org.allenai.nlpstack.parse.poly.polyparser.{ InMemoryPolytreeParseSource, PolytreeParseSource, PolytreeParse }
import org.allenai.nlpstack.parse.poly.reranking.ParseNodeFeature

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
    goldParses: Iterator[PolytreeParse], statistics: Seq[ParseStatistic]
  ) {

    for {
      (candidateParse, goldParse) <- candidateParses.zip(goldParses)
      stat <- statistics
    } stat.notify(candidateParse, goldParse)
    for {
      stat <- statistics
    } stat.report()
  }
}

/** A ParseStatistic accrues a particular statistic over (candidate parse, gold parse) pairs.
  * Every time .notify() is called, the statistic is updated.
  */
sealed abstract class ParseStatistic {
  def reset(): Unit = {}

  def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit

  /** Display a report about the accumulated statistics to stdout. */
  def report(): Unit
}

/** UnlabeledBreadcrumbAccuracy stores the statistics necessary to compute Unlabeled
  * Attachment Score (UAS), which is the percentage of correct breadcrumbs over a set
  * of candidate parses.
  */
case object UnlabeledBreadcrumbAccuracy extends ParseStatistic {
  var numCorrect = 0
  var numLabeledCorrect = 0
  var numTotal = 0
  var numCorrectNoPunc = 0
  var numLabeledCorrectNoPunc = 0
  var numTotalNoPunc = 0
  var numParses = 0

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    numParses += 1
    numTotal += goldParse.breadcrumb.tail.size
    candidateParse match {
      case Some(candParse) =>
        if (candParse.breadcrumb.size == goldParse.breadcrumb.size) {
          // skip the first element because it is the nexus (hence it has no breadcrumb)
          val zipped = candParse.breadcrumb.zipWithIndex.tail.zip(
            goldParse.breadcrumb.tail
          )
          val nonPuncZipped = zipped filter {
            case ((x, i), y) =>
              goldParse.tokens(i).getDeterministicProperty('cpos) != Symbol(".")
          }
          numCorrect += candParse.breadcrumb.tail.zip(goldParse.breadcrumb.tail) count
            { case (x, y) => (x == y) }
          numLabeledCorrect += candParse.breadcrumb.zipWithIndex.tail.zip(
            goldParse.breadcrumb.tail
          ) count {
            case ((x, i), y) => (x == y) &&
              candParse.arcLabelByEndNodes.getOrElse(Set(x, i), 'nomatch) ==
              goldParse.arcLabelByEndNodes(Set(y, i))
          }
          numTotalNoPunc += nonPuncZipped.size
          numCorrectNoPunc += nonPuncZipped count {
            case ((x, _), y) => (x == y)
          }
          numLabeledCorrectNoPunc += nonPuncZipped count {
            case ((x, i), y) => (x == y) &&
              candParse.arcLabelByEndNodes.getOrElse(Set(x, i), 'nomatch) ==
              goldParse.arcLabelByEndNodes(Set(y, i))
          }
        } else { // skip the parse if the tokenization is different
          println(s"WARNING -- Skipping parse: ${candParse.sentence.asWhitespaceSeparatedString}" +
            s" tokenized differently than gold: ${goldParse.sentence.asWhitespaceSeparatedString}")
          numParses -= 1
          numTotal -= goldParse.breadcrumb.tail.size
        }
      case None =>
        println(s"WARNING -- Failed parse")
    }
  }

  override def report(): Unit = {
    println("UAS: %d / %d = %2.2f%%".format(numCorrect, numTotal,
      (100.0 * numCorrect) / numTotal))
    println("UAS (no punc): %d / %d = %2.2f%%".format(numCorrectNoPunc, numTotalNoPunc,
      (100.0 * numCorrectNoPunc) / numTotalNoPunc))
    println("LAS: %d / %d = %2.2f%%".format(numLabeledCorrect, numTotal,
      (100.0 * numLabeledCorrect) / numTotal))
    println("LAS (no punc): %d / %d = %2.2f%%".format(numLabeledCorrectNoPunc, numTotalNoPunc,
      (100.0 * numLabeledCorrectNoPunc) / numTotalNoPunc))
  }

  override def reset(): Unit = {
    numCorrect = 0
    numLabeledCorrect = 0
    numTotal = 0
    numCorrectNoPunc = 0
    numLabeledCorrectNoPunc = 0
    numTotalNoPunc = 0
    numParses = 0
  }
}

/** FirstMistakeAggregator collates the "first" incorrect transition decisions that a parser
  * makes with respect to gold parses. For instance, say there's a parse whose gold transition
  * representation is [Sh, Sh, Rt('nsubj), Rt('det)], and a candidate parse comes up with
  * [Sh, Rt('adjp), Sh, Rt('det)]. The "first" incorrect transition decision is the pair
  * (Rt('adjp), Sh). In other words, the first mistake that the candidate parse made was to
  * guess transition Rt('adjp) when it should have guessed Sh.
  */
/*
case object FirstMistakeAggregator extends ParseStatistic {
  var mistakeHistogram = Map[(StateTransition, StateTransition), Int]()

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    for {
      candParse <- candidateParse
      candidateTransitions <- candParse.asTransitionSequence
      goldTransitions <- goldParse.asTransitionSequence
      zippedTransitions = candidateTransitions.zip(goldTransitions)
      firstMistake <- zippedTransitions find { case (t1, t2) => t1 != t2 }
    } {
      mistakeHistogram = mistakeHistogram +
        ((firstMistake._1, firstMistake._2) -> (1 + mistakeHistogram.getOrElse((firstMistake._1,
          firstMistake._2), 0)))
    }
  }

  override def report(): Unit = {}
}
*/

case class MistakeAnalyzer(classifier: WrapperClassifier, feature: ParseNodeFeature)
    extends ParseStatistic {

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    if (candidateParse != None) {
      val scoringFunction = PathAccuracyScore(
        InMemoryPolytreeParseSource(Seq(goldParse)),
        ignorePunctuation = false, ignorePathLabels = false
      )
      scoringFunction.getRatio(candidateParse.get) match {
        case (correctIncrement, totalIncrement) =>
          if (totalIncrement > 0 && correctIncrement.toFloat / totalIncrement < 0.5) {
            println(s"sentence: ${goldParse.sentence.asWhitespaceSeparatedString}")
            println(s"candidate: ${candidateParse.get}")
            println(s"gold: $goldParse")
            val goldNodeWeirdness = Range(0, goldParse.tokens.size) map { tokenIndex =>
              (tokenIndex, classifier.getDistribution(feature(goldParse, tokenIndex)).getOrElse(0, 0.0))
            }
            val weirdGoldNodes = (goldNodeWeirdness filter { x => x._2 >= 0.5 }) map { _._1 }
            weirdGoldNodes foreach { node => println(s"Weird gold node: $node") }

            val candidateNodeWeirdness = Range(0, candidateParse.get.tokens.size) map { tokenIndex =>
              (tokenIndex, classifier.getDistribution(feature(candidateParse.get, tokenIndex)).getOrElse(0, 0.0))
            }
            val weirdCandidateNodes = (candidateNodeWeirdness filter { x => x._2 >= 0.5 }) map { _._1 }
            weirdCandidateNodes foreach { node => println(s"Weird candidate node: $node") }

            /*
            val weirdDescendants = Range(0, candidateParse.get.tokens.size) filter { tokenIndex =>
              (candidateParse.get.paths(tokenIndex).toSet & weirdCandidateNodes.toSet).nonEmpty
            }
            weirdDescendants foreach { node => println(s"Weird descendant: $node") }
            */
            println("")
          }
      }
    }
  }

  override def report(): Unit = {
  }

  override def reset(): Unit = {
  }
}

/** A ParseScore maps a candidate parse to a score. */
trait ParseScore extends (PolytreeParse => Double) {
  def getRatio(candidateParse: PolytreeParse): (Int, Int)
}

case class PathAccuracy(ignorePunctuation: Boolean, ignorePathLabels: Boolean)
    extends ParseStatistic {

  var numCorrect = 0
  var numTotal = 0
  var numParses = 0

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    numParses += 1
    if (candidateParse != None) {
      val scoringFunction = PathAccuracyScore(
        InMemoryPolytreeParseSource(Seq(goldParse)),
        ignorePunctuation, ignorePathLabels
      )
      scoringFunction.getRatio(candidateParse.get) match {
        case (correctIncrement, totalIncrement) =>
          numCorrect += correctIncrement
          numTotal += totalIncrement
      }
    }
  }

  override def report(): Unit = {
    val puncNote = Map(true -> "ignorePunc", false -> "full")
    val labelNote = Map(true -> "unlabeled", false -> "labeled")
    println(s"Path Accuracy (${labelNote(ignorePathLabels)}, ${puncNote(ignorePunctuation)}): " +
      s"%d / %d = %2.2f%%".format(numCorrect, numTotal,
        (100.0 * numCorrect) / numTotal))
  }

  override def reset(): Unit = {
    numCorrect = 0
    numTotal = 0
    numParses = 0
  }
}

/** The PathAccuracyScore computes the percentage of a candidate parse's tokens that have a
  * completely correct breadcrumb path (i.e. if you follow a token's breadcrumbs to the nexus
  * in both the candidate and the gold parse, you encounter the same set of tokens in the same
  * order).
  */
case class PathAccuracyScore(
    goldParseSource: PolytreeParseSource,
    ignorePunctuation: Boolean, ignorePathLabels: Boolean
) extends ParseScore {

  private val goldParses: Map[String, PolytreeParse] = (goldParseSource.parseIterator map { parse =>
    (parse.sentence.asWhitespaceSeparatedString, parse)
  }).toMap

  def getRatio(candParse: PolytreeParse): (Int, Int) = {
    goldParses.get(candParse.sentence.asWhitespaceSeparatedString) match {
      case Some(goldParse) =>
        if (candParse.breadcrumb.size != goldParse.breadcrumb.size ||
          goldParse.breadcrumb.size <= 1) {

          println(s"WARNING -- Skipping parse: ${candParse.sentence.asWhitespaceSeparatedString}" +
            s" tokenized differently than gold: ${goldParse.sentence.asWhitespaceSeparatedString}")
          (0, 0)
        } else {
          // skip the first element because it is the nexus (hence it has no breadcrumb)
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
          val numCorrect: Int = zippedPaths count {
            case ((x, i), y) =>
              (x == y) &&
                (ignorePathLabels ||
                  ((x :+ i) map { tokIndex => candParse.breadcrumbArcLabel(tokIndex) }) ==
                  ((y :+ i) map { tokIndex => goldParse.breadcrumbArcLabel(tokIndex) }))
          }
          (numCorrect, zippedPaths.size)
        }
      case None => (0, 0) // TODO: throw error?
    }
  }

  final def apply(candParse: PolytreeParse): Double = {
    val (numerator, denominator) = getRatio(candParse)
    if (denominator == 0) {
      0.0
    } else {
      numerator.toFloat / denominator
    }
  }
}
