package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.testkit.UnitSpec

class DecisionTreeSpec extends UnitSpec {

  val decisionTree1 = DecisionTree(
    outcomes = Seq(0, 1),
    child = Vector(
      Map(0 -> 1, 1 -> 2), // node 0
      Map(), // node 1
      Map(0 -> 3, 1 -> 4), // node 2
      Map(), // node 3
      Map()
    ), // node 4
    splittingFeature = Vector(
      Some(35), // node 0
      None, // node 1
      Some(20), // node 2
      None, // node 3
      None
    ), // node 4
    outcomeHistograms = Vector(
      Map(0 -> 45, 1 -> 55), // node 0
      Map(0 -> 29, 1 -> 9).toMap, // node 1
      Map(0 -> 16, 1 -> 46).toMap, // node 2
      Map(0 -> 5, 1 -> 10).toMap, // node 3
      Map(0 -> 11, 1 -> 36).toMap
    ) // node 4
  )

  "DecisionTree.outcomeHistogram" should "get node 3's histogram" in {
    val fv = SparseVector(outcome = None, numFeatures = 100, trueFeatures = Set(34, 35))
    decisionTree1.outcomeHistogram(fv) shouldBe Map(0 -> 5, 1 -> 10)
  }

  it should "get node 1's histogram" in {
    val fv = SparseVector(outcome = None, numFeatures = 100, trueFeatures = Set(34, 20))
    decisionTree1.outcomeHistogram(fv) shouldBe Map(0 -> 29, 1 -> 9)
  }

  "DecisionTree.allFeatures" should "return 20 and 35 for decisionTree1" in {
    decisionTree1.allFeatures shouldBe Set(20, 35)
  }

  "DecisionTree.outcomeDistribution" should "get node 3's add-one smoothed distribution" in {
    val fv = SparseVector(outcome = None, numFeatures = 100, trueFeatures = Set(34, 20))
    decisionTree1.outcomeDistribution(fv) shouldBe Map(0 -> 0.75, 1 -> 0.25)
  }
}
