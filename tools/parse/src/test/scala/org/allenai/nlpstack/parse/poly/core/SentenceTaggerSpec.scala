package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.testkit.UnitSpec

case object ToyTagger extends SentenceTagger {

  private val featureName = 'place
  private val hasNexusSymbol = 'nexus
  private val hasFirstSymbol = 'first
  private val hasSecondSymbol = 'second
  private val hasSecondLastSymbol = 'secondLast
  private val hasLastSymbol = 'last

  override def tag(sentence: Sentence): TaggedSentence = {
    TaggedSentence(
      sentence,
      (Range(0, sentence.tokens.size) map {
      case tokenIndex =>
        (
          tokenIndex,
          Set(
            if (tokenIndex == 0) Some(TokenTag(featureName, hasNexusSymbol)) else None,
            if (tokenIndex == 1) Some(TokenTag(featureName, hasFirstSymbol)) else None,
            if (tokenIndex == 2) Some(TokenTag(featureName, hasSecondSymbol)) else None,
            if (tokenIndex == sentence.size - 2) Some(TokenTag(featureName, hasSecondLastSymbol)) else None,
            if (tokenIndex == sentence.size - 1) Some(TokenTag(featureName, hasLastSymbol)) else None
          ).flatten
        )
    }).toMap
    )
  }
}

class SentenceTaggerSpec extends UnitSpec {
  // scalastyle:off

  private val keywordTagger =
    KeywordTaggerInitializer(Set("apple", "blueberry", "cherry")).initialize()

  "tagWithMultipleTaggers" should "give the correct tags" in {
    val sent = Sentence.initializeFromWhitespaceSeparatedString("apple and blueberry pie")
    SentenceTagger.tagWithMultipleTaggers(sent, Seq(ToyTagger, keywordTagger)).tags shouldBe
      Map(
        0 -> Set(TokenTag('place, 'nexus)),
        1 -> Set(TokenTag('place, 'first), TokenTag('keyword, 'apple)),
        2 -> Set(TokenTag('place, 'second)),
        3 -> Set(TokenTag('place, 'secondLast), TokenTag('keyword, 'blueberry)),
        4 -> Set(TokenTag('place, 'last))
      )
  }

  private val tokenPositionTagger = TokenPositionTaggerInitializer.initialize()

  "TokenPositionTagger" should "give the correct tags" in {
    val sent = Sentence.initializeFromWhitespaceSeparatedString("apple and a blueberry pie")
    SentenceTagger.tagWithMultipleTaggers(sent, Seq(tokenPositionTagger)).tags shouldBe
      Map(
        0 -> Set(TokenTag('place, 'nexus)),
        1 -> Set(TokenTag('place, 'first)),
        2 -> Set(TokenTag('place, 'second)),
        3 -> Set(),
        4 -> Set(TokenTag('place, 'secondLast)),
        5 -> Set(TokenTag('place, 'last))
      )
  }

  it should "give multiple tags to the same token when a sentence is short" in {
    val sent = Sentence.initializeFromWhitespaceSeparatedString("apple pie")
    SentenceTagger.tagWithMultipleTaggers(sent, Seq(tokenPositionTagger)).tags shouldBe
      Map(
        0 -> Set(TokenTag('place, 'nexus)),
        1 -> Set(TokenTag('place, 'first), TokenTag('place, 'secondLast)),
        2 -> Set(TokenTag('place, 'second), TokenTag('place, 'last))
      )
  }

  it should "correctly handle very short sentences" in {
    val sent = Sentence.initializeFromWhitespaceSeparatedString("pie")
    SentenceTagger.tagWithMultipleTaggers(sent, Seq(tokenPositionTagger)).tags shouldBe
      Map(
        0 -> Set(TokenTag('place, 'nexus), TokenTag('place, 'secondLast)),
        1 -> Set(TokenTag('place, 'first), TokenTag('place, 'last))
      )
  }
}
