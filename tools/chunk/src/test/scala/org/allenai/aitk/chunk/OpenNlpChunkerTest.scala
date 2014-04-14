package org.allenai.aitk
package chunk

import org.allenai.aitk.tokenize._
import org.allenai.aitk.postag._

import org.allenai.common.testkit.UnitSpec

class OpenNlpChunkerTest extends UnitSpec {
  "chunker" should "correctly chunk an example sentence" in {
    val text = "This is a test of the OpenNlp chunker."
    val tokenizer = new SimpleEnglishTokenizer
    val postagger = new OpenNlpPostagger
    val chunker = new OpenNlpChunker

    val chunked = chunker.chunk(tokenizer, postagger)(text)
    chunked.mkString("; ") === "This 0 DT B-NP; is 5 VBZ B-VP; a 8 DT B-NP; test 10 NN I-NP; of 15 IN B-PP; the 18 DT B-NP; OpenNlp 22 NNP I-NP; chunker 30 NN I-NP; . 37 . O"
  }
}

