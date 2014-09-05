package org.allenai.nlpstack.segment

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.Segment

import org.apache.commons.io.IOUtils

class FactorieSegmenterSpec extends UnitSpec {
  val sentencer = new FactorieSegmenter
  val document = "He went to work.  He bought a first-class suit. He ate a melon."

  "factorie sentencer" should "properly segment" in {
    val segments = sentencer.segment(document).toIndexedSeq
    assert(segments(0) === Segment("He went to work.", 0))
    assert(segments(1) === Segment("He bought a first-class suit.", 18))
    assert(segments(2) === Segment("He ate a melon.", 48))
  }

  it should "not throw an exception for a long string" in {
    val s =
      IOUtils.toString(
        this.getClass.getResourceAsStream("/org/allenai/nlpstack/segment/unclosed_tag_test.txt"),
        "UTF-8")
    sentencer.segment(s)
  }

  it should "not interpret dollar symbols as regex backreferences" in {
    val s = "<" + "$2" + "x" * 98
    sentencer.segment(s)
  }

}