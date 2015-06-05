package org.allenai.nlpstack.parse.poly.core

import org.allenai.nlpstack.parse.poly.fsm.{ Sculpture, SculptureSource }

/** A TaggedSentence is a sentence accompanied by a map that assigns tags to its tokens.
  *
  * Specifically, the `tags` field maps each token index to a set of TokenTag objects corresponding
  * to that token.
  *
  * @param sentence the untagged sentence
  * @param tags maps each token index to a set of TokenTag objects
  */
case class TaggedSentence(sentence: Sentence, tags: Map[Int, Set[TokenTag]]) extends Sculpture {
  override val marbleBlock = sentence
}

/** A data source for TaggedSentence objects. */
trait TaggedSentenceSource extends SculptureSource with SentenceSource {
  def taggedSentenceIterator: Iterator[TaggedSentence]

  override def sculptureIterator: Iterator[Sculpture] = taggedSentenceIterator

  override def sentenceIterator: Iterator[Sentence] = taggedSentenceIterator map { taggedSentence =>
    taggedSentence.sentence
  }
}

/** A TaggedSentenceSource derived from a SentenceSource.
  *
  * Tokens are tagged with a specified property from their `properties` field.
  *
  * @param sentenceSource the sentence source to derive the tagged sentences from
  * @param propertyName the token property to use as the "tag"
  */
case class DerivedTaggedSentenceSource(
    sentenceSource: SentenceSource,
    propertyName: Symbol
) extends TaggedSentenceSource {

  override def taggedSentenceIterator: Iterator[TaggedSentence] = {
    for {
      sentence <- sentenceSource.sentenceIterator
    } yield {
      TaggedSentence(
        sentence,
        (sentence.tokens.zipWithIndex map {
        case (tok, index) =>
          (index, tok.getProperty(propertyName) map { prop => TokenTag(propertyName, prop) })
      }).toMap
      )
    }
  }
}
