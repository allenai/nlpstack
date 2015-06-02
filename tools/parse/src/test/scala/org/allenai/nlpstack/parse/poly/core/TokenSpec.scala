package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.testkit.UnitSpec

class TokenSpec extends UnitSpec {
  // scalastyle:off

  ".getProperty" should "return the empty set for an undefined property" in {
    Token.create("the").getProperty('unknownProperty) shouldBe Set()
  }

  ".getDeterministicProperty" should "return the correct answer" in {
    val tok = Token.create("the", coarsePos = Some("DET"), finePos = Some("DT"))
    tok.updateProperty('myProperty, Set('good))
    tok.getDeterministicProperty('cpos) shouldEqual 'DET
  }

  it should "return Token.propertyNotFound" in {
    val tok = Token.create("the").updateProperty('definite, Set('yes))
    tok.getDeterministicProperty('indefinite) shouldEqual Token.propertyNotFound
  }

  ".updateProperty" should "override the previous value" in {
    val tok = Token.create("the").updateProperty('definite, Set('yes))
    tok.getDeterministicProperty('definite) shouldEqual Symbol("yes")
    tok.updateProperty('definite, Set('no)).getDeterministicProperty('definite) shouldEqual 'no
  }

  ".extendProperty" should "extend the previous value" in {
    val tok = Token.create("the").extendProperty('definite, 'yes)
    tok.getProperty('definite) shouldBe Set('yes)
    tok.extendProperty('definite, 'no).getProperty('definite) shouldBe Set('yes, 'no)
  }

}
