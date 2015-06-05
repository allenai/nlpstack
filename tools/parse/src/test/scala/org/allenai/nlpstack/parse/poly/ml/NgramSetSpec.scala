package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Token, Sentence }
import java.io.File

class NgramSetSpec extends UnitSpec {

  /*
  val ngramSet1 = NgramSet.initializeFromUnderscoreSeparatedTerms(
    Seq("graduated_cylinder", "Bunsen_burner", "oneword")
  )


  "BrownClusters.getAllClusters" should "return the correct answer" in {
    val sentence = Sentence.initializeFromWhitespaceSeparatedString(
      "My Bunsen burner is better than Bunsen and his graduated cylinder"
    )
    ngramSet1.identifyNgrams(sentence) shouldBe Set((2, 4), (10, 12))
  }

  it should "correctly handle one-word ngrams" in {
    val sentence = Sentence.initializeFromWhitespaceSeparatedString(
      "This oneword should be easy ."
    )
    ngramSet1.identifyNgrams(sentence) shouldBe Set((2, 3))
  }

  it should "correctly handle the beginning-of-sentence edge case" in {
    val sentence = Sentence.initializeFromWhitespaceSeparatedString(
      "Bunsen burner ? I hardly know her ."
    )
    println(s"ngramSet: ${ngramSet1.prefixes}")
    ngramSet1.identifyNgrams(sentence) shouldBe Set((1, 3))
  }
  */
}
