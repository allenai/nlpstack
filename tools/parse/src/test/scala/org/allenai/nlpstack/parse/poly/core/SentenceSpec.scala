package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.immutable.Interval
import org.allenai.common.testkit.UnitSpec

class SentenceSpec extends UnitSpec {
  // scalastyle:off

  ".initializeFromWhitespaceSeparatedString" should "give the correct sentence" in {
    Sentence.initializeFromWhitespaceSeparatedString("This is input .") shouldBe
      Sentence(IndexedSeq(NexusToken, Token('This), Token('is), Token('input), Token(Symbol("."))))
  }

  it should "ignore leading and trailing whitespace" in {
    Sentence.initializeFromWhitespaceSeparatedString("  This is input .   ") shouldBe
      Sentence(IndexedSeq(NexusToken, Token('This), Token('is), Token('input), Token(Symbol("."))))
  }

  "Initializing a sentence" should "give the correct paren intervals for sent1" in {
    val sent1 = Sentence.initializeFromWhitespaceSeparatedString("we saw black cats")
    sent1.parenIntervals shouldBe Set.empty
  }

  it should "give the correct paren intervals for sent2" in {
    val sent2 = Sentence.initializeFromWhitespaceSeparatedString(
      "with the help of animals ( insects and birds ) flowers can be pollinated ( fertilized ) ."
    )
    sent2.parenIntervals shouldBe Set(Interval.closed(6, 10), Interval.closed(15, 17))
  }

  it should "give the correct paren intervals for sent3" in {
    val sent3 = Sentence.initializeFromWhitespaceSeparatedString(
      "with the help of animals ( ( insects ) and birds ) flowers can " +
        "be pollinated ( fertilized ) ."
    )
    sent3.parenIntervals shouldBe
      Set(Interval.closed(7, 9), Interval.closed(6, 12), Interval.closed(17, 19))
  }

  it should "give the correct paren intervals for sent4" in {
    val sent4 = Sentence.initializeFromWhitespaceSeparatedString(
      "with the help of animals insects ) and birds ) flowers can " +
        "be pollinated ( fertilized ."
    )
    sent4.parenIntervals shouldBe
      Set(Interval.closed(0, 7), Interval.closed(0, 10), Interval.closed(15, 17))
  }
}
