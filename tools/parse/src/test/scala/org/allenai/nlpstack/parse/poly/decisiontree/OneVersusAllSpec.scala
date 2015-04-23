package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.testkit.UnitSpec

/** This MockBinaryProbabilisticClassifiers accords four different binary distributions depending
  * on whether the specified features are present in a feature vector (in priority order).
  *
  * @param feature1 the highest priority feature
  * @param feature2 the second-highest priority feature
  * @param feature3 the third-highest priority feature
  * @param feature4 the fourth-highest priority feature
  */
case class MockBinaryProbabilisticClassifier(feature1: Int, feature2: Int,
    feature3: Int, feature4: Int) extends ProbabilisticClassifier {

  override def classify(featureVector: FeatureVector): (Int, Option[Justification]) = {
    val (bestClass, _) = outcomeDistribution(featureVector) maxBy { case (_, prob) => prob }
    (bestClass, None)
  }

  /** Gets the probability distribution over outcomes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return probability distribution of outcomes according to training data
    */
  override def outcomeDistribution(featureVector: FeatureVector): Map[Int, Float] = {
    if (featureVector.nonzeroFeatures.contains(feature1)) {
      Map(0 -> 0.2f, 1 -> 0.8f)
    } else if (featureVector.nonzeroFeatures.contains(feature2)) {
      Map(0 -> 0.4f, 1 -> 0.6f)
    } else if (featureVector.nonzeroFeatures.contains(feature3)) {
      Map(0 -> 0.6f, 1 -> 0.4f)
    } else if (featureVector.nonzeroFeatures.contains(feature4)) {
      Map(0 -> 0.8f, 1 -> 0.2f)
    } else {
      Map(0 -> 1.0f, 1 -> 0.0f)
    }
  }

  /** All features used by the classifier. */
  def allFeatures: Set[Int] = Set(feature1, feature2, feature3, feature4)
}

class OneVersusAllSpec extends UnitSpec {

  val classifier = OneVersusAll(binaryClassifiers =
    Seq(
      (0, MockBinaryProbabilisticClassifier(0, 1, 2, 3)),
      (1, MockBinaryProbabilisticClassifier(1, 3, 0, 2)),
      (2, MockBinaryProbabilisticClassifier(3, 2, 1, 0))
    ))

  val classifier2 = OneVersusAll(binaryClassifiers =
    Seq(
      (0, MockBinaryProbabilisticClassifier(0, 1, 2, 3)),
      (1, MockBinaryProbabilisticClassifier(1, 3, 0, 2)),
      (2, MockBinaryProbabilisticClassifier(3, 2, 1, 0)),
      (3, MockBinaryProbabilisticClassifier(2, 3, 1, 0))
    ))

  "OneVersusAll.outcomeDistribution" should "return the right answer" in {
    val outcomeDist = classifier.outcomeDistribution(new SparseVector(
      outcome = None,
      numFeatures = 5, trueFeatures = Set(1, 2)
    ))
    outcomeDist(0) shouldBe 0.3f +- 0.001f
    outcomeDist(1) shouldBe 0.4f +- 0.001f
    outcomeDist(2) shouldBe 0.3f +- 0.001f
  }

  it should "return a uniform distribution if all subclassifiers return zero" in {
    classifier2.outcomeDistribution(new SparseVector(outcome = None, numFeatures = 5,
      trueFeatures = Set(4))) shouldBe Map(0 -> 0.25f, 1 -> 0.25f, 2 -> 0.25f, 3 -> 0.25f)
  }
}
