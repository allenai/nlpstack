package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.testkit.UnitSpec

class TrainingDataSpec extends UnitSpec {

  val nameA = FeatureName(List('a))
  val nameB = FeatureName(List('b))
  val nameC = FeatureName(List('c))

  val vec1 = FeatureVector(Seq(nameA -> 6.0, nameB -> -5.0, nameC -> -3.0))
  val vec2 = FeatureVector(Seq(nameA -> -2.0, nameC -> 4.0))

  "Calling .binarize" should "return the correct value" in {
    val data = TrainingData(Seq((vec1, 0.7), (vec2, -0.5)))
    data.binarize(0.4) shouldBe TrainingData(Seq((vec1, 1.0), (vec2, -1.0)))
  }

  "Calling .binarize" should "filter values smaller than the margin" in {
    val data = TrainingData(Seq((vec1, 0.7), (vec2, -0.5)))
    data.binarize(0.6) shouldBe TrainingData(Seq((vec1, 1.0)))
  }
}
