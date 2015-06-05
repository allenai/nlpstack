package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.polyparser.{ FileBasedPolytreeParseSource, PolytreeParseSource, PolytreeParseFileFormat }

import scala.compat.Platform

object TaggingEvaluation {

  def fullTaggingEvaluation(
    tagger: SentenceTagger,
    testFiles: String,
    testFileFormat: PolytreeParseFileFormat,
    dataSource: String,
    oracleNbestSize: Int
  ): Unit = {

    val testSources: Map[String, PolytreeParseSource] = {
      testFiles.split(",") map { path =>
        (path, FileBasedPolytreeParseSource.getParseSource(
          path,
          testFileFormat, dataSource
        ))
      }
    }.toMap
    for ((sourcePath, testSource) <- testSources) {
      println(s"Checking tagging accuracy on test set $sourcePath.")
      evaluateTaggerOnTestSet(tagger, DerivedTaggedSentenceSource(testSource, Token.coarsePos))
    }
  }

  def evaluateTaggerOnTestSet(
    tagger: SentenceTagger,
    goldSentenceSource: TaggedSentenceSource
  ): Unit = {

    println("Tagging test set.")
    val startTime: Long = Platform.currentTime
    val candidateTaggedSentences: Iterator[TaggedSentence] =
      SentenceTagger.tagSentenceSource(tagger, goldSentenceSource)
    val scoringFunction = PostagAccuracyScore(goldSentenceSource)

    val overallRatio = (candidateTaggedSentences map { candidateSent =>
      scoringFunction.getRatio(candidateSent)
    }) reduce { (x, y) => (x._1 + y._1, x._2 + y._2) }
    val parsingDurationInSeconds: Double = (Platform.currentTime - startTime) / 1000.0
    val numParses = goldSentenceSource.sentenceIterator.size
    println(s"Accuracy: ${overallRatio._1 / overallRatio._2}")
    println("Parsed %d sentences in %.1f seconds, an average of %.1f sentences per second.".format(
      numParses, parsingDurationInSeconds,
      numParses.toDouble / parsingDurationInSeconds
    ))
  }

}

/** A TaggedSentenceScore maps a tagged sentence to a score. */
abstract class TaggedSentenceScore extends (TaggedSentence => Double) {

  def apply(candSentence: TaggedSentence): Double = {
    val (numerator, denominator) = getRatio(candSentence)
    if (denominator == 0) {
      0.0
    } else {
      numerator.toDouble / denominator
    }
  }

  /** Returns a ratio, typically correctEvents / totalEvents.
    *
    * @param candidateSentence the tagged sentence to evaluate
    * @return a ratio, typically correctEvents / totalEvents
    */
  def getRatio(candidateSentence: TaggedSentence): (Int, Int)

  /** A friendly name for this scoring function. */
  def name: String
}

/** Counts the fraction of correctly labeled coarse part-of-speech tags in a candidate parse.
  *
  * @param goldSentences the gold TaggedSentences to compare against
  */
case class PostagAccuracyScore(goldSentences: TaggedSentenceSource)
    extends TaggedSentenceScore {

  override val name = "Tagging accuracy"

  override def getRatio(candSentence: TaggedSentence): (Int, Int) = {
    val candSentenceKey = getTaggedSentenceKey(candSentence)
    require(goldBank.contains(candSentenceKey))
    val goldSentence = goldBank(candSentenceKey)
    val numCorrect =
      goldSentence.tags.toSeq map {
        case (tokIndex, goldTags) =>
          (goldTags, candSentence.tags.getOrElse(tokIndex, Set[TokenTag]()))
      } count {
        case (goldTags, candTags) =>
          goldTags == candTags
      }
    (numCorrect, goldSentence.tags.size)
  }

  private val goldBank: Map[String, TaggedSentence] =
    (goldSentences.taggedSentenceIterator map { goldSent =>
      (getTaggedSentenceKey(goldSent), goldSent)
    }).toMap

  private def getTaggedSentenceKey(taggedSentence: TaggedSentence): String = {
    taggedSentence.sentence.asWhitespaceSeparatedString
  }
}
