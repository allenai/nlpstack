package org.allenai.nlpstack.tokenize

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.Tokenizer

import org.apache.commons.io.IOUtils

abstract class TokenizerSpec extends UnitSpec {
  def tokenizerToTest: Tokenizer

  val testSentences = Seq(
    """|The battle station is heavily shielded and carries a firepower greater
       |than half the star fleet. Its defenses are designed around a direct,
       |large-scale assault. A small one-man fighter should be able to
       |penetrate the outer defense.""".stripMargin,
    """|Pardon me for asking, sir, but what good are snub fighters going to be
       |against that?""".stripMargin)

  val tokenizedTestSentences = Seq(
    """|The 0
       |battle 4
       |station 11
       |is 19
       |heavily 22
       |shielded 30
       |and 39
       |carries 43
       |a 51
       |firepower 53
       |greater 63
       |than 71
       |half 76
       |the 81
       |star 85
       |fleet 90
       |. 95
       |Its 97
       |defenses 101
       |are 110
       |designed 114
       |around 123
       |a 130
       |direct 132
       |, 138
       |large-scale 140
       |assault 152
       |. 159
       |A 161
       |small 163
       |one-man 169
       |fighter 177
       |should 185
       |be 192
       |able 195
       |to 200
       |penetrate 203
       |the 213
       |outer 217
       |defense 223
       |. 230""".stripMargin,
    """|Pardon 0
       |me 7
       |for 10
       |asking 14
       |, 20
       |sir 22
       |, 25
       |but 27
       |what 31
       |good 36
       |are 41
       |snub 45
       |fighters 50
       |going 59
       |to 65
       |be 68
       |against 71
       |that 79
       |? 83""".stripMargin)

  "tokenizer implementation" should "correctly tokenize two example sentences" in {
    for ((text, expected) <- testSentences zip tokenizedTestSentences) {
      val tokenized = tokenizerToTest.tokenize(text)
      val tokenizedString = tokenized.mkString("\n")
      assert(tokenizedString === expected)
    }
  }

  it should "not throw an exception for a long string" in {
    val s =
      IOUtils.toString(
        this.getClass.getResourceAsStream("/org/allenai/nlpstack/tokenize/unclosed_tag_test.txt"),
        "UTF-8")
    tokenizerToTest.tokenize(s)
  }

  it should "not throw an exception with unclosed tags" in {
    tokenizerToTest.tokenize("ab <" + ("xxx " * (2000 / 4)))
    tokenizerToTest.tokenize("ab <" + ("xxx " * (100 / 4)) + "x") // 101 characters after the <
    tokenizerToTest.tokenize("ab <" + ("xxx " * (100 / 4))) // 100 characters after the <
    tokenizerToTest.tokenize("ab <" + ("xxxxxxxx " * (99 / 9))) // 99 characters after the <
    tokenizerToTest.tokenize("< foo < bar")
  }

  it should "correctly deal with hyphens" in {
    {
      val ts = tokenizerToTest.tokenize("-1 is a small number")
      assert(ts.length === 5)
      assert(ts(0).string === "-1")
      assert(ts(0).offset === 0)
    }

    {
      val ts = tokenizerToTest.tokenize("a space-based adventure")
      assert(ts.length === 3)
      assert(ts(1).string === "space-based")
      assert(ts(1).offset === 2)
    }

    {
      val ts = tokenizerToTest.tokenize("rose- and sky-tinted glasses")
      assert(ts.length === 4)
      assert(ts(0).string === "rose-")
      assert(ts(0).offset === 0)
      assert(ts(2).string === "sky-tinted")
      assert(ts(2).offset === 10)
    }

    {
      val ts = tokenizerToTest.tokenize("-")
      assert(ts.length === 1)
      assert(ts(0).string === "-")
      assert(ts(0).offset === 0)
    }

    {
      val ts = tokenizerToTest.tokenize("don't go-")
      assert(ts.length === 3)
      assert(ts(2).string === "go-")
      assert(ts(2).offset === 6)
    }

    {
      val ts = tokenizerToTest.tokenize("He said no - only to agree with me later.")
      assert(ts.length === 11)
      assert(ts(3).string === "-")
      assert(ts(3).offset === 11)
    }

    {
      val ts = tokenizerToTest.tokenize("We're having a tête-à-tête in the conference room.")
      assert(ts.length === 10)
      assert(ts(4).string === "tête-à-tête")
      assert(ts(4).offset === 15)
    }

    {
      val ts = tokenizerToTest.tokenize("The score is -1-ish")
      assert(ts.length === 4)
      assert(ts(3).string === "-1-ish")
      assert(ts(3).offset === 13)
    }
  }
}
