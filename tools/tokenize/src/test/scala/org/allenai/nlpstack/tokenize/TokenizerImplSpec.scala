package org.allenai.nlpstack
package chunk

import org.allenai.nlpstack.tokenize._
import org.allenai.nlpstack.postag._

import org.allenai.common.testkit.UnitSpec

class TokenizerImplSpec extends UnitSpec {
  "tokenizer implementation" should "correctly tokenize an example sentence" in {
    val text = "This is a test of the OpenNlp chunker."
    val tokenizer = new SimpleEnglishTokenizer

    val tokenized = tokenizer.tokenize(text)
    tokenized.mkString("; ") === "This 0; is 5; a 8; test 10; of 15; the 18; OpenNlp 22; chunker 30; . 37"
  }
}

