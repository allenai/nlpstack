package org.allenai.nlpstack.parse.poly.polytagger

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.Sentence

class PostaggerStateRefSpec extends UnitSpec {
  // scalastyle:off

  "OffsetStateRef" should "return the right token" in {
    val sentence = Sentence.initializeFromWhitespaceSeparatedString(
      "a1 b2 c3 d4 e5 f6 g7 h8 i9 j10"
    )
    OffsetRef(3)(PostaggerState(Some(4), existingTags = Map(), sentence)).toSet shouldBe Set(7)
    OffsetRef(-2)(PostaggerState(Some(4), existingTags = Map(), sentence)).toSet shouldBe Set(2)
    OffsetRef(0)(PostaggerState(Some(4), existingTags = Map(), sentence)).toSet shouldBe Set(4)
    OffsetRef(6)(PostaggerState(Some(4), existingTags = Map(), sentence)).toSet shouldBe Set(10)
    OffsetRef(-4)(PostaggerState(Some(4), existingTags = Map(), sentence)).toSet shouldBe Set(0)
  }

  it should "return an empty set if the token doesn't exist" in {
    val sentence = Sentence.initializeFromWhitespaceSeparatedString(
      "a1 b2 c3 d4 e5 f6 g7 h8 i9 j10"
    )
    OffsetRef(7)(PostaggerState(Some(4), existingTags = Map(), sentence)).toSet shouldBe Set()
    OffsetRef(-5)(PostaggerState(Some(4), existingTags = Map(), sentence)).toSet shouldBe Set()
  }
}
