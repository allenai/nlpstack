package org.allenai.aitk
package postag

import org.allenai.aitk.tokenize._
import org.allenai.common.testkit.UnitSpec

object OpenNlpPostaggerTest extends UnitSpec {
  "OpenNLP pos-tagger" should "correctly postag an example sentence" in {
    val text = "This is a test of the OpenNlp postagger."
    val tokenizer = new SimpleEnglishTokenizer
    val postagger = new OpenNlpPostagger

    val postagged = postagger.postag(tokenizer)(text)
    postagged.mkString("; ") === "This 0 DT; is 5 VBZ; a 8 DT; test 10 NN; of 15 IN; the 18 DT; OpenNlp 22 NNP; postagger 30 NN; . 39 ."
  }
}

