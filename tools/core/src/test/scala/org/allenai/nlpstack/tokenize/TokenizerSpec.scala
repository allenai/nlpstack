package org.allenai.nlpstack.tokenize

import org.allenai.common.testkit.UnitSpec

class TokenizerSpecTest extends UnitSpec {
  "tokenizer" should "compute offsets correctly and infer the original text" in {
    val sentence = "John walks down the hall."
    val tokens = Tokenizer.computeOffsets(Seq("John", "walks", "down", "the", "hall", "."), sentence)

    // make sure offsets were computed correctly
    assert(tokens.map(_.offsets.start) === Seq(0, 5, 11, 16, 20, 24))

    // make sure we can go back to the original sentence
    assert(Tokenizer.originalText(tokens) === sentence)
  }

  it should "compute offsets correctly and infer the original text when there is a leading space" in {
    val sentence = "  John walks down the hall."
    val tokens = Tokenizer.computeOffsets(Seq("John", "walks", "down", "the", "hall", "."), sentence)

    // make sure offsets were computed correctly
    assert(tokens.map(_.offsets.start) === Seq(2, 7, 13, 18, 22, 26))

    // make sure we can go back to the original sentence
    assert(Tokenizer.originalText(tokens) === sentence)
  }

  it should "trim original text correctly when a start offset is specified" in {
    val sentence = "  John walks down the hall."
    val trimmedSentence = "John walks down the hall."
    val tokens = Tokenizer.computeOffsets(Seq("John", "walks", "down", "the", "hall", "."), sentence)

    // make sure offsets were computed correctly
    assert(tokens.map(_.offsets.start) === Seq(2, 7, 13, 18, 22, 26))

    // make sure we can go back to the original sentence
    assert(Tokenizer.originalText(tokens, tokens.head.offset) === trimmedSentence)
  }

  it should "throw an exception if tokens are out of order" in {
    val tokens = Seq(
      new Token("large-scale", 0),
      new Token("large", 0),
      new Token("scale", 6))

    a[IllegalArgumentException] should be thrownBy {
      Tokenizer.originalText(tokens, 10)
    }
  }
}
