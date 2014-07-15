package org.allenai.nlpstack.core.chunk

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.postag._
import org.allenai.nlpstack.core.tokenize._

class ChunkerSpec extends UnitSpec {
  "chunker" should "determine intervals correctly." in {
    val strings = "John very quickly ran away from the deep blue reflection in the mirror .".split(" ")
    val postags = "NNP RB RB VBD RB IN DT JJ JJ NN IN DT NN .".split(" ")
    val chunks = "B-NP B-ADVP B-ADVP B-VP B-ADVP B-PP B-NP I-NP I-NP I-NP B-PP B-NP I-NP O".split(" ")

    val text = "John very quickly ran away from the deep blue reflection in the mirror."
    val tokens = Tokenizer.computeOffsets(strings, text)
    val chunkedTokens = Chunker.tokensFrom(chunks, postags, tokens)

    Chunker.intervals(chunkedTokens).map(_.toString) should contain theSameElementsAs (
      List(
        "(NP,{0})",
        "(ADVP,{1})",
        "(ADVP,{2})",
        "(VP,{3})",
        "(ADVP,{4})",
        "(PP,{5})",
        "(NP,[6, 10))",
        "(PP,{10})",
        "(NP,[11, 13))",
        "(O,{13})"))
  }

  it should "join of" in {
    val strings = "John 's dog ate at the University of Washington".split(" ")
    val postags = "NNP POS NN VBD IN DT NNP IN NNP".split(" ")
    val chunks = "B-NP B-NP I-NP V-BP B-PP B-NP I-NP B-PP B-NP".split(" ")

    val text = "John's dog ate at the University of Washington."
    val tokens = Tokenizer.computeOffsets(strings, text)
    val chunkedTokens = Chunker.tokensFrom(chunks, postags, tokens)

    Chunker.joinOf(chunkedTokens).map(_.chunk).mkString(" ") ===
      "B-NP B-NP I-NP V-BP B-PP B-NP I-NP I-NP I-NP"
  }

  it should "join possessives" in {
    val strings = "John 's dog ate at the University of Washington".split(" ")
    val postags = "NNP POS NN VBD IN DT NNP IN NNP".split(" ")
    val chunks = "B-NP B-NP I-NP V-BP B-PP B-NP I-NP B-PP B-NP".split(" ")

    val text = "John's dog ate at the University of Washington."
    val tokens = Tokenizer.computeOffsets(strings, text)
    val chunkedTokens = Chunker.tokensFrom(chunks, postags, tokens)

    Chunker.joinPos(chunkedTokens).map(_.chunk).mkString(" ") ===
      "B-NP I-NP I-NP V-BP B-PP B-NP I-NP B-PP B-NP"
  }
}
