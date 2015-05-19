package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.fsm.{ SculptureSource, MarbleBlock, Sculpture }
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParseSource

/** A TaggedSentence is a sentence whose tokens are tagged with a set of symbols.
  *
  * @param sentence the unannotated sentence
  * @param tags a map from token indices (according to sentence.tokens) to symbol sets
  */
case class TaggedSentence(sentence: Sentence, tags: Map[Int, Set[Symbol]]) extends Sculpture {
  val marbleBlock: MarbleBlock = sentence

  /** Adds the tags to the properties field of the sentence tokens.
    *
    * @param tagName the property name we want to give to the tags
    * @return the original sentence, where the token properties are augmented with the tags
    */
  def addTagsToSentenceProperties(tagName: Symbol): Sentence = {
    val tokensAndTags = Range(0, sentence.size) map { tokIndex =>
      (sentence.tokens(tokIndex), tags.get(tokIndex))
    }
    Sentence(tokensAndTags map {
      case (token, Some(tag)) =>
        token.updateProperties(Map(tagName -> tag))
      case (token, None) =>
        token
    })
  }
}

/** A data source for TaggedSentence objects. */
trait TaggedSentenceSource extends SculptureSource {
  def taggedSentenceIterator: Iterator[TaggedSentence]

  override def sculptureIterator: Iterator[Sculpture] = taggedSentenceIterator
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
          (index, tok.getProperty(propertyName))
      }).toMap
      )
    }
  }
}
