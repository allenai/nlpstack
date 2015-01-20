package org.allenai.nlpstack.parse.poly.decisiontree

/** The OneVersusAll implements multi-outcome classification as a set of binary classifiers.
  *
  * A ProbabilisticClassifier is associated with each outcome. Suppose there are three
  * outcomes: 0, 1, 2. Then the constructor would take a sequence of three classifiers
  * as its argument: [(0,A), (1,B), (2,C)]. To compute the outcome distribution for
  * a new feature vector v, the OneVersusAll would normalize:
  *
  * [ A.outcomeDistribution(v)(1), B.outcomeDistribution(v)(1), C.outcomeDistribution(v)(1) ]
  *
  * i.e. the probability of 1 (true) according to binary classifiers A, B, and C.
  *
  * QUESTION(MH): is this the best way to normalize these, or would it be better to normalize
  * by summing the logs and then re-applying the exponential operation?
  *
  * @param binaryClassifiers the binary classifier associated with each outcome
  */
case class OneVersusAll(binaryClassifiers: Seq[(Int, ProbabilisticClassifier)])
    extends ProbabilisticClassifier {

  override def outcomeDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    val unnormalizedDist: Seq[(Int, Double)] =
      binaryClassifiers map {
        case (outcome, classifier) =>
          (outcome, classifier.outcomeDistribution(featureVector).getOrElse(1, 0.0))
      }
    ProbabilisticClassifier.normalizeDistribution(unnormalizedDist).toMap
  }

  /** All features used by at least one of the binary subclassifiers. */
  override def allFeatures: Set[Int] = {
    binaryClassifiers map {
      case (_, classifier) =>
        classifier.allFeatures
    } reduce { (x, y) => x union y }
  }
}

/** A OneVersusAllTrainer trains a OneVersusAll using a base ProbabilisticClassifierTrainer to
  * train one binary classifiers per outcome.
  *
  * @param baseTrainer the trainer to use for training the binary classifiers
  */
class OneVersusAllTrainer(baseTrainer: ProbabilisticClassifierTrainer)
    extends ProbabilisticClassifierTrainer {

  override def apply(data: FeatureVectors): ProbabilisticClassifier = {
    val binaryClassifiers = data.allOutcomes.toSet.toSeq map { outcome: Int =>
      val mappedVectors = data.featureVectors map { featureVector =>
        featureVector.modifyOutcome(
          featureVector.outcome match {
            case Some(x) if x == outcome => 1
            case _ => 0
          }
        )
      }
      (outcome, baseTrainer(FeatureVectors(mappedVectors)))
    }
    OneVersusAll(binaryClassifiers)
  }
}
