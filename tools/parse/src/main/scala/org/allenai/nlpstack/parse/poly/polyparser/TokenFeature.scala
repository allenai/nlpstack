package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, Sentence, Token }
import org.allenai.nlpstack.parse.poly.fsm.{ State, StateFeature }
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, FeatureName }
import org.allenai.nlpstack.parse.poly.postagging.{ TokenTag, TaggedSentence }

import reming.LazyFormat
import reming.DefaultJsonProtocol._

object TokenFeatureAnnotator {

  def annotate(sentence: Sentence, tagging: TaggedSentence): AnnotatedSentence = {
    AnnotatedSentence(
      sentence,
      Range(0, sentence.size) map { tokenIndex =>
        FeatureVector(
          tagging.tags.getOrElse(tokenIndex, Set[TokenTag]()).toSeq
            map { tag =>
              FeatureName(Seq(tag.name, tag.value)) -> 1.0
            }
        )
      }
    )
  }
}

sealed abstract class TokenFeature extends ((Sentence, Int) => Seq[(FeatureName, Double)]) {

  /** If the sentence has a token at position `tokenIndex`, then this returns the
    * singleton sequence containing that token. Otherwise, this returns an empty sequence.
    *
    * @param sentence the sentence
    * @param tokenIndex the index of the desired token in the sentence
    * @return a singleton sequence containing the specified token (if it exists); otherwise
    * an empty sequence
    */
  protected final def getTokenSequence(sentence: Sentence, tokenIndex: Int): Seq[Token] = {
    Seq(sentence.tokens.lift(tokenIndex)).flatten
  }
}

object TokenFeature {
  implicit object TokenFeatureJsonFormat extends LazyFormat[TokenFeature] {
    private implicit val wordFeatureFormat = jsonFormat0(() => WordFeature)
    private implicit val tokenPositionFeatureFormat = jsonFormat0(() => TokenPositionFeature)
    private implicit val tokenPropertyFeatureFormat = jsonFormat1(TokenPropertyFeature.apply)
    private implicit val keywordFeatureFormat = jsonFormat1(KeywordFeature.apply)
    private implicit val suffixFeatureFormat = jsonFormat1(SuffixFeature.apply)
    private implicit val prefixFeatureFormat = jsonFormat1(PrefixFeature.apply)

    override val delegate = parentFormat[TokenFeature](
      childFormat[WordFeature.type, TokenFeature],
      childFormat[TokenPositionFeature.type, TokenFeature],
      childFormat[TokenPropertyFeature, TokenFeature],
      childFormat[KeywordFeature, TokenFeature],
      childFormat[SuffixFeature, TokenFeature],
      childFormat[PrefixFeature, TokenFeature]
    )
  }
}

/** The WordFeature maps a token to its word representation.
  *
  * See the definition of TokenFeature (above) for more details about the interface.
  *
  */
case object WordFeature extends TokenFeature {

  @transient val featureName: Symbol = 'wordIs

  override def apply(sentence: Sentence, tokenIndex: Int): Seq[(FeatureName, Double)] = {
    getTokenSequence(sentence, tokenIndex) map { token =>
      FeatureName(List(featureName, token.word)) -> 1.0
    }
  }

}

/** The TokenPropertyFeature maps a token to one of its properties.
  *
  * See the definition of TokenFeature (above) for more details about the interface.
  *
  */
case class TokenPropertyFeature(property: Symbol)
    extends TokenFeature {

  @transient val featureName: Symbol = 'tokProp

  override def apply(sentence: Sentence, tokenIndex: Int): Seq[(FeatureName, Double)] = {
    getTokenSequence(sentence, tokenIndex) flatMap { token =>
      token.getProperty(property).toSeq map { propValue =>
        FeatureName(Seq(property, propValue)) -> 1.0
      }
    }
  }
}

/** The KeywordFeature maps a token to its word representation, if its word appears in the
  * argument set `keywords`. Otherwise its apply function will return an empty set.
  *
  * See the definition of TokenFeature (above) for more details about the interface.
  *
  */
case class KeywordFeature(keywords: Set[Symbol]) extends TokenFeature {

  @transient val featureName: Symbol = 'keywordIs

  override def apply(sentence: Sentence, tokenIndex: Int): Seq[(FeatureName, Double)] = {
    getTokenSequence(sentence, tokenIndex) flatMap { token =>
      val word = Symbol(token.word.name.toLowerCase)
      if (keywords.contains(word)) {
        Seq(FeatureName(List(featureName, word)) -> 1.0)
      } else {
        Seq[(FeatureName, Double)]()
      }
    }
  }
}

/** The SuffixFeature maps a token to the set of its suffixes that are contained in a
  * set of "key" suffixes.
  *
  * See the definition of TokenFeature (above) for more details about the interface.
  *
  * @param keysuffixes the set of suffixes to treat as "key" suffixes
  *
  */
case class SuffixFeature(keysuffixes: Seq[Symbol]) extends TokenFeature {
  @transient val featureName: Symbol = 'suffixIs

  override def apply(sentence: Sentence, tokenIndex: Int): Seq[(FeatureName, Double)] = {
    getTokenSequence(sentence, tokenIndex) flatMap { token =>
      val word = token.word.name.toLowerCase
      keysuffixes filter { suffix =>
        word.endsWith(suffix.name.toLowerCase)
      } map { suffix =>
        FeatureName(List(featureName, suffix)) -> 1.0
      }
    }
  }
}

/** The PrefixFeature maps a token to the set of its prefixes that are contained in a
  * set of "key" prefixes.
  *
  * See the definition of TokenFeature (above) for more details about the interface.
  *
  * @param keyprefixes the set of prefixes to treat as "key" prefixes
  *
  */
case class PrefixFeature(keyprefixes: Seq[Symbol]) extends TokenFeature {
  @transient val featureName: Symbol = 'prefixIs

  override def apply(sentence: Sentence, tokenIndex: Int): Seq[(FeatureName, Double)] = {
    getTokenSequence(sentence, tokenIndex) flatMap { token =>
      val word = token.word.name.toLowerCase
      keyprefixes filter { prefix =>
        word.startsWith(prefix.name.toLowerCase)
      } map { prefix =>
        FeatureName(List(featureName, prefix)) -> 1.0
      }
    }
  }
}

case object TokenPositionFeature extends TokenFeature {

  @transient val featureName = 'place
  @transient val hasNexusSymbol = 'nexus
  @transient val hasFirstSymbol = 'first
  @transient val hasSecondSymbol = 'second
  @transient val hasSecondLastSymbol = 'secondLast
  @transient val hasLastSymbol = 'last

  override def apply(sentence: Sentence, tokenIndex: Int): Seq[(FeatureName, Double)] = {
    val indexProperties: Seq[Symbol] =
      Seq(
        if (tokenIndex == 0) Some(hasNexusSymbol) else None,
        if (tokenIndex == 1) Some(hasFirstSymbol) else None,
        if (tokenIndex == 2) Some(hasSecondSymbol) else None,
        if (tokenIndex == sentence.size - 2) Some(hasSecondLastSymbol) else None,
        if (tokenIndex == sentence.size - 1) Some(hasLastSymbol) else None
      ).flatten
    indexProperties map { property =>
      FeatureName(List(featureName, property)) -> 1.0
    }
  }
}
