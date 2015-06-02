package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.core.{ TokenTag, TaggedSentenceSource, TaggedSentence }

/** A TaggedSentenceScore maps a tagged sentence to a score. */
abstract class TaggedSentenceScore extends (TaggedSentence => Double) {

  def apply(candSentence: TaggedSentence): Double = {
    val (numerator, denominator) = getRatio(candSentence)
    if (denominator == 0) {
      0.0
    } else {
      numerator.toFloat / denominator
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
