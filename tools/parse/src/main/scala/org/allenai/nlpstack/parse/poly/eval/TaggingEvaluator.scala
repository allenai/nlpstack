package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.core.{ TaggedSentence, Sentence }
import org.allenai.nlpstack.parse.poly.polyparser.{ ArcLabel, PolytreeParseSource, InMemoryPolytreeParseSource, PolytreeParse }

object TaggingEvaluator {

  def evaluate(
    candidates: Iterator[TaggedSentence],
    golds: Iterator[TaggedSentence], statistics: Seq[EvaluationStatistic]
  ) {

    for {
      (candidate, gold) <- candidates.zip(golds)
      stat <- statistics
    } stat.notify(candidate, gold)
    for {
      stat <- statistics
    } stat.report()
  }
}

/** A ParseStatistic accrues a particular statistic over (candidate parse, gold parse) pairs.
  * Every time .notify() is called, the statistic is updated.
  */
abstract class EvaluationStatistic {
  def reset(): Unit = {}

  def notify(candidate: TaggedSentence, gold: TaggedSentence): Unit

  /** Display a report about the accumulated statistics to stdout. */
  def report(): Unit
}

case class CposSentAccuracy(verbose: Boolean = false) extends EvaluationStatistic {
  var numCorrect = 0
  var numTotal = 0
  var numParses = 0
  var numGuesses = 0

  override def notify(cand: TaggedSentence, gold: TaggedSentence): Unit = {
    numParses += 1
    numTotal += gold.sentence.tokens.tail.size
    // skip the first element because it is the nexus (hence it has no part-of-speech)
    numCorrect +=
      Range(1, gold.sentence.tokens.size) count { tokIndex =>
        val goldTag: Set[Symbol] = gold.tags(tokIndex)
        require(goldTag.size == 1, s"Gold tagged sentence should only have one gold tag for ${gold.sentence.tokens(tokIndex)}: $gold")
        val candidateTags = cand.tags(tokIndex)
        if (!candidateTags.contains(goldTag.head)) {
          println(s"Wrong tags for ${cand.sentence.tokens(tokIndex).word.name}: $candidateTags; Should be: ${goldTag.head}; ${cand.sentence.asWhitespaceSeparatedString}")
        }
        candidateTags.contains(goldTag.head)
      }
    numGuesses +=
      (Range(1, gold.sentence.tokens.size) map { tokIndex =>
        cand.tags(tokIndex).size
      }).sum
  }

  override def report(): Unit = {
    println("Cpos Tagging: %d / %d = %2.2f%%".format(numCorrect, numTotal,
      (100.0 * numCorrect) / numTotal))
    println("Cpos Ambiguity: %d / %d = %2.2f".format(numGuesses, numTotal,
      (1.0 * numGuesses) / numTotal))
  }

  override def reset(): Unit = {
    numCorrect = 0
    numTotal = 0
    numParses = 0
    numGuesses = 0
  }
}
