package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.testkit.UnitSpec

class TokenTaggerSpec extends UnitSpec {
  // scalastyle:off

  "LexicalPropertiesTagger" should "give the correct tags" in {
    val sent = Sentence.initializeFromWhitespaceSeparatedString("apple and blueberry pie")
    LexicalPropertiesTagger.tag(Token(Symbol("hello"))) shouldBe Set()
    LexicalPropertiesTagger.tag(Token(Symbol("Hello"))) shouldBe Set(
      TokenTag(LexicalPropertiesTagger.taggerName, 'firstCap),
      TokenTag(LexicalPropertiesTagger.taggerName, 'existsCap)
    )
    LexicalPropertiesTagger.tag(Token(Symbol("HELLO"))) shouldBe Set(
      TokenTag(LexicalPropertiesTagger.taggerName, 'firstCap),
      TokenTag(LexicalPropertiesTagger.taggerName, 'existsCap),
      TokenTag(LexicalPropertiesTagger.taggerName, 'allCaps)
    )
    LexicalPropertiesTagger.tag(Token(Symbol("HELLO22"))) shouldBe Set(
      TokenTag(LexicalPropertiesTagger.taggerName, 'firstCap),
      TokenTag(LexicalPropertiesTagger.taggerName, 'existsCap),
      TokenTag(LexicalPropertiesTagger.taggerName, 'existsNum)
    )
  }
}
