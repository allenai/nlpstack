package org.allenai
package aitk
package tokenize

import org.allenai.aitk.graph.Graph._
import org.allenai.aitk.stem.Stemmer
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
}
