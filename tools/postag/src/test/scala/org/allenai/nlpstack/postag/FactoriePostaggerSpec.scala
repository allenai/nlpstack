package org.allenai.nlpstack.postag

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.tokenize._

class FactoriePostaggerSpec extends UnitSpec {
  "Factorie pos-tagger" should "correctly postag an example sentence" in {
    val text = "This is a test of the OpenNlp postagger."
    val tokenizer = defaultTokenizer
    val postagger = new FactoriePostagger

    val postagged = postagger.postag(tokenizer)(text)
    assert(postagged.mkString("; ") === "This 0 DT; is 5 VBZ; a 8 DT; test 10 NN; of 15 IN; the 18 DT; OpenNlp 22 NNP; postagger 30 NN; . 39 .")
  }
}
