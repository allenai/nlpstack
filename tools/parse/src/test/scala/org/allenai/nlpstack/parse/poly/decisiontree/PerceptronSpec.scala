package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.testkit.UnitSpec

class PerceptronSpec extends UnitSpec {

  "Perceptron.outcomeDistribution" should "get the correct distribution" in {
    val classifier = Perceptron(Seq(
      (20, Vector(0.4, 1.2)),
      (34, Vector(1.6, -1.3)),
      (50, Vector(0.5, -2.3))
    ))
    val fv = SparseVector(outcome = None, numFeatures = 100, trueFeatures = Set(34, 20))
    classifier.outcomeDistribution(fv)(0) shouldBe 2.0 +- 0.001
    classifier.outcomeDistribution(fv)(1) shouldBe -0.1 +- 0.001
  }

}
