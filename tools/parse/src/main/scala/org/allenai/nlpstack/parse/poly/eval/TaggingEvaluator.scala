package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.polyparser.{ ArcLabel, PolytreeParseSource, InMemoryPolytreeParseSource, PolytreeParse }

object TaggingEvaluator {

  def evaluate(
    candidates: Iterator[Option[Sentence]],
    golds: Iterator[Sentence], statistics: Seq[EvaluationStatistic]
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

  def notify(candidate: Option[Sentence], gold: Sentence): Unit

  /** Display a report about the accumulated statistics to stdout. */
  def report(): Unit
}

case class CposSentAccuracy(verbose: Boolean = false) extends EvaluationStatistic {
  var numCorrect = 0
  var numTotal = 0
  var numParses = 0
  var errorHistogram = Map[(Symbol, Symbol), Int]()

  override def notify(candidate: Option[Sentence], gold: Sentence): Unit = {
    candidate match {
      case Some(cand) =>
        numParses += 1
        numTotal += gold.tokens.tail.size
        // skip the first element because it is the nexus (hence it has no part-of-speech)
        numCorrect +=
          cand.tokens.tail.zip(gold.tokens.tail) count {
            case (candToken, goldToken) =>
              candToken.getDeterministicProperty('cpos) ==
                goldToken.getDeterministicProperty('cpos)
          }
        cand.tokens.tail.zip(gold.tokens.tail) filter {
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
      case None =>
        numParses += 1
        numTotal += gold.tokens.tail.size
        println(s"WARNING -- Failed parse")
    }
  }

  override def report(): Unit = {
    println("Cpos Tagging: %d / %d = %2.2f%%".format(numCorrect, numTotal,
      (100.0 * numCorrect) / numTotal))
    if (verbose) {
      errorHistogram.toSeq sortBy {
        case (_, count) =>
          count
      } foreach {
        case ((candCpos, goldCpos), count) =>
          println(s"  $count: ${goldCpos.name} ~~> ${candCpos.name}")
      }
    }
  }

  override def reset(): Unit = {
    numCorrect = 0
    numTotal = 0
    numParses = 0
  }
}
