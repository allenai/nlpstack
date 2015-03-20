package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.testkit.UnitSpec
import spray.json._

class FeatureVectorSpec extends UnitSpec {
  // scalastyle:off

  val nameA = FeatureName(List('a))
  val nameB = FeatureName(List('b))
  val nameC = FeatureName(List('c))

  "Calling .getFeatureValue" should "return the correct value" in {
    val vec1 = FeatureVector(Seq(nameA -> 0.5, nameB -> 0.3))
    vec1.getFeatureValue(nameA) shouldBe 0.5
    vec1.getFeatureValue(nameB) shouldBe 0.3
  }

  it should "return zero for an unrecognized feature name" in {
    val vec1 = FeatureVector(Seq(nameA -> 0.5, nameB -> 0.3))
    vec1.getFeatureValue(nameC) shouldBe 0
  }

  "Calling subtractVectors" should "return the correct difference vector" in {
    FeatureVector.subtractVectors(
      FeatureVector(Seq(nameA -> 1, nameB -> 5)),
      FeatureVector(Seq(nameA -> 3, nameC -> 4))
    ).featureMap shouldBe
      Map(nameA -> -2, nameB -> 5, nameC -> -4)
  }

  "Calling mergeVectors" should "prioritize mappings in the first vector" in {
    FeatureVector.mergeVectors(
      FeatureVector(Seq(nameA -> 0.5, nameB -> 0.3)),
      FeatureVector(Seq(nameA -> 0.7, nameC -> 0.4))
    ).featureMap shouldBe
      Map(nameA -> 0.5, nameB -> 0.3, nameC -> 0.4)
  }

  "Serializing a FeatureVector" should "preserve the vector" in {
    val vec1 = FeatureVector(Seq(nameA -> 0.5, nameB -> 0.3))
    vec1.toJson.convertTo[FeatureVector] shouldBe vec1
  }
}
