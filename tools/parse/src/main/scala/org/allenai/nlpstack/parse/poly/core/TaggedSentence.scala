package org.allenai.nlpstack.parse.poly.core

import org.allenai.nlpstack.parse.poly.fsm.{ Sculpture, SculptureSource }
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParseSource

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

/** A TaggedSentenceSource derived from a PolytreeParseSource.
  *
  * Tokens are tagged with a specified property from their `properties` field.
  *
  * @param parseSource the parse source to derive the tagged sentences from
  * @param propertyName the token property to use as the "tag"
  */
case class ParseDerivedTaggedSentenceSource(
    parseSource: PolytreeParseSource,
    propertyName: Symbol
) extends TaggedSentenceSource {

  override def taggedSentenceIterator: Iterator[TaggedSentence] = {
    for {
      sentence <- parseSource.sentenceIterator
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
