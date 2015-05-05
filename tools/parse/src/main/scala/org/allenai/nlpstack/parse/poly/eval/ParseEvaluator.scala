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
}

/** A ParseStatistic accrues a particular statistic over (candidate parse, gold parse) pairs.
  * Every time .notify() is called, the statistic is updated.
  */
abstract class ParseStatistic {
  def reset(): Unit = {}

  def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit

  /** Display a report about the accumulated statistics to stdout. */
  def report(statWriter: Option[PrintWriter]): Unit

  def logMessage(statWriter: Option[PrintWriter], message: String): Unit = {
    statWriter match {
      case Some(w) => w.println(message)
      case None => println(message)
    }
  }
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

  override def notify(
    candidateParse: Option[PolytreeParse], goldParse: PolytreeParse
  ): Unit = {
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
          println(s"WARNING -- Skipping parse: " +
            s"${candParse.sentence.asWhitespaceSeparatedString}" +
            s" tokenized differently than gold: " +
            s"${goldParse.sentence.asWhitespaceSeparatedString}")
          numParses -= 1
          numTotal -= goldParse.breadcrumb.tail.size
        }
      case None =>
        println(s"WARNING -- Failed parse")
    }
  }

  override def report(statWriter: Option[PrintWriter] = None): Unit = {
    logMessage(
      statWriter,
      "UAS: %d / %d = %2.2f%%".format(numCorrect, numTotal, (100.0 * numCorrect) / numTotal)
    )
    logMessage(
      statWriter,
      "UAS (no punc): %d / %d = %2.2f%%".format(numCorrectNoPunc, numTotalNoPunc,
        (100.0 * numCorrectNoPunc) / numTotalNoPunc)
    )
    logMessage(
      statWriter,
      "LAS: %d / %d = %2.2f%%".format(numLabeledCorrect, numTotal,
        (100.0 * numLabeledCorrect) / numTotal)
    )
    logMessage(
      statWriter,
      "LAS (no punc): %d / %d = %2.2f%%".format(numLabeledCorrectNoPunc, numTotalNoPunc,
        (100.0 * numLabeledCorrectNoPunc) / numTotalNoPunc)
    )
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
          numTotal += goldParse.breadcrumb.tail.size
          // skip the first element because it is the nexus (hence it has no part-of-speech)
          numCorrect +=
            candParse.tokens.tail.zip(goldParse.tokens.tail) count {
              case (candToken, goldToken) =>
                candToken.getDeterministicProperty('cpos) ==
                  goldToken.getDeterministicProperty('cpos)
            }
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

  override def report(statWriter: Option[PrintWriter]): Unit = {
    logMessage(
      statWriter, "Cpos Tagging: %d / %d = %2.2f%%".format(numCorrect, numTotal,
      (100.0 * numCorrect) / numTotal)
    )
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

case class PathAccuracy(ignorePunctuation: Boolean, ignorePathLabels: Boolean)
    extends ParseStatistic {

  var numCorrect = 0
  var numTotal = 0
  var numParses = 0

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    numParses += 1
    val scoringFunction = PathAccuracyScore(
      InMemoryPolytreeParseSource(Seq(goldParse)),
      ignorePunctuation, ignorePathLabels
    )
    candidateParse map { parse =>
      scoringFunction.getRatio(parse)
    } foreach {
      case (correctIncrement, totalIncrement) =>
        numCorrect += correctIncrement
        numTotal += totalIncrement
    }
  }

  override def report(statWriter: Option[PrintWriter]): Unit = {
    val puncNote = Map(true -> "ignorePunc", false -> "full")
    val labelNote = Map(true -> "unlabeled", false -> "labeled")
    logMessage(
      statWriter,
      s"Path Accuracy (${labelNote(ignorePathLabels)}, ${puncNote(ignorePunctuation)}): " +
        s"%d / %d = %2.2f%%".format(numCorrect, numTotal,
          (100.0 * numCorrect) / numTotal)
    )
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
            case ((candidatePath, token), goldPath) =>
              (candidatePath == goldPath) &&
                (ignorePathLabels ||
                  convertPathToArcLabels(candidatePath :+ token, candParse) ==
                  convertPathToArcLabels(goldPath :+ token, goldParse))
          }
          (numCorrect, zippedPaths.size)
        }
      case None => (0, 0) // TODO: throw error?
    }
  }

  private val goldParses: Map[String, PolytreeParse] = (goldParseSource.parseIterator map { parse =>
    (parse.sentence.asWhitespaceSeparatedString, parse)
  }).toMap

  /** Converts a path in the parse tree to its arc labels. */
  private def convertPathToArcLabels(path: Seq[Int], parse: PolytreeParse): Seq[ArcLabel] = {
    path map { pathToken =>
      parse.breadcrumbArcLabel(pathToken)
    }
  }
}
