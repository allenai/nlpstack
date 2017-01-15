package org.allenai.nlpstack.headword

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

class KnowitallHeadExtractorSpec extends UnitSpec {

  val knowitallHeadExtractor = new KnowitallHeadExtractor()

  def getHeadStringNp(phrase: String): String = {
    (knowitallHeadExtractor.argumentHead(defaultTokenizer, defaultPostagger)(phrase)
      .map(_.string)).mkString(" ")
  }

  def getHeadStringVp(phrase: String): String = {
    (knowitallHeadExtractor.relationHead(defaultTokenizer, defaultPostagger)(phrase)
      .map(_.string)).mkString(" ")
  }

  val testNp1 = "the ten green elephants"
  val testNp2 = "good coffee"
  val testNp3 = "Barack Obama"
  val testNp4 = "group of animals"
  val testNp5 = "10 policemen"

  val testNp1Result = getHeadStringNp(testNp1)
  val testNp2Result = getHeadStringNp(testNp2)
  val testNp3Result = getHeadStringNp(testNp3)
  val testNp4Result = getHeadStringNp(testNp4)
  val testNp5Result = getHeadStringNp(testNp5)

  val testNp1ExpectedHeadStr = "elephants"
  val testNp2ExpectedHeadStr = "coffee"
  val testNp3ExpectedHeadStr = "Barack Obama"
  val testNp4ExpectedHeadStr = "animals"
  val testNp5ExpectedHeadStr = "policemen"

  val testVp1 = "walking quickly"
  val testVp2 = "quickly walked"
  val testVp3 = "are trying"
  val testVp4 = "might eat"
  val testVp5 = "can't eat"

  val testVp1Result = getHeadStringVp(testVp1)
  val testVp2Result = getHeadStringVp(testVp2)
  val testVp3Result = getHeadStringVp(testVp3)
  val testVp4Result = getHeadStringVp(testVp4)
  val testVp5Result = getHeadStringVp(testVp5)

  val testVp1ExpectedHeadStr = "walking"
  val testVp2ExpectedHeadStr = "walked"
  val testVp3ExpectedHeadStr = "are trying"
  val testVp4ExpectedHeadStr = "eat"
  val testVp5ExpectedHeadStr = "eat"

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

  it should
    "return the right head word for verb phrases" in {
      assert(
        (testVp1Result === testVp1ExpectedHeadStr) &&
          (testVp2Result === testVp2ExpectedHeadStr) &&
          (testVp3Result === testVp3ExpectedHeadStr) &&
          (testVp4Result === testVp4ExpectedHeadStr) &&
          (testVp5Result === testVp5ExpectedHeadStr)
      )
    }

  it should "throw an exception if wordnet path is invalid" in {
    a[IllegalArgumentException] should be thrownBy {
      new KnowitallHeadExtractor("foo/bar")
    }
  }
}
