package org.allenai.nlpstack.segment

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.segment.Segment

class FactorieSegmenterSpec extends UnitSpec {
  val sentencer = new FactorieSegmenter
  val document = "He went to work.  He bought a first-class suit. He ate a melon."
  "chalk sentencer" should "properly segment" in {
    val segments = sentencer.segment(document).toIndexedSeq
    assert(segments(0) === Segment("He went to work.", 0))
    assert(segments(1) === Segment("He bought a first-class suit.", 18))
    assert(segments(2) === Segment("He ate a melon.", 48))
  }
}
