package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.testkit.UnitSpec

class LinearModelSpec extends UnitSpec {

  val nameA = FeatureName(List('a))
  val nameB = FeatureName(List('b))
  val nameC = FeatureName(List('c))
  val model1 = LinearModel(Seq((nameA, -2.0), (nameB, 3.0)))

  "Calling .getCoefficient" should "return the correct value" in {
    model1.getCoefficient(nameA) shouldBe -2.0
    model1.getCoefficient(nameB) shouldBe 3.0
  }

  it should "return zero for unspecified coefficients" in {
    model1.getCoefficient(nameC) shouldBe 0
  }

  "Calling .score" should "return the correct score" in {
    model1.score(FeatureVector(Seq(nameA -> 6.0, nameB -> 5.0, nameC -> -3.0))) shouldBe 3.0
  }
}
