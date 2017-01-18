package org.allenai.nlpstack.headword

import org.allenai.common.testkit.UnitSpec

class JwiToolsSpec extends UnitSpec {

  val jwiTools = new JwiTools()

  "JwiTools" should "correctly stem a word" in {
    val word = "elephants"
    val stem = jwiTools.stem(word)
    assert(stem === "elephant")
  }

  it should "throw an exception if wordnet path is invalid" in {
    a[IllegalArgumentException] should be thrownBy {
      new JwiTools("foo/bar")
    }
  }
}
