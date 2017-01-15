package org.allenai.nlpstack.headword

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

class KnowitallHeadExtractorSpec extends UnitSpec {

  val knowitallHeadExtractor = new KnowitallHeadExtractor()

  val testNp1 = "the ten green elephants"
  val testNp2 = "good coffee"
  val testNp3 = "Barack Obama"
  val testNp4 = "group of animals"
  val testNp5 = "10 policemen"

  def getHeadString(phrase: String): String = {
    (knowitallHeadExtractor.argumentHead(defaultTokenizer, defaultPostagger)(phrase)
      .map(_.string)).mkString(" ")
  }
  val testNp1Result = getHeadString(testNp1)
  val testNp2Result = getHeadString(testNp2)
  val testNp3Result = getHeadString(testNp3)
  val testNp4Result = getHeadString(testNp4)
  val testNp5Result = getHeadString(testNp5)

  val testNp1ExpectedHeadStr = "elephants"
  val testNp2ExpectedHeadStr = "coffee"
  val testNp3ExpectedHeadStr = "Barack Obama"
  val testNp4ExpectedHeadStr = "animals"
  val testNp5ExpectedHeadStr = "policemen"

  "KnowitallHeadExtractor" should
    "return the right head word for noun phrases" in {
      assert(
        (testNp1Result === testNp1ExpectedHeadStr) &&
          (testNp2Result === testNp2ExpectedHeadStr) &&
          (testNp3Result === testNp3ExpectedHeadStr) &&
          (testNp4Result === testNp4ExpectedHeadStr) &&
          (testNp5Result === testNp5ExpectedHeadStr)
      )
    }

  // TODO: Add tests for relationHead.

  it should "throw an exception if wordnet path is invalid" in {
    a[IllegalArgumentException] should be thrownBy {
      new KnowitallHeadExtractor("foo/bar")
    }
  }
}
