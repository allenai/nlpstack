package org.allenai.nlpstack.parse.poly.decisiontree

case class OneVersusAll(binaryClassifiers: Seq[(Int, ProbabilisticClassifier)])
    extends ProbabilisticClassifier {

  override def outcomeDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    val unnormalizedDist: Seq[(Int, Double)] =
      binaryClassifiers map {
        case (outcome, classifier) =>
          (outcome, classifier.outcomeDistribution(featureVector).getOrElse(1, 0.0000001))
      }
    ProbabilisticClassifier.normalizeDistribution(unnormalizedDist).toMap
  }

  override def allFeatures: Set[Int] = {
    binaryClassifiers map {
      case (_, classifier) =>
        classifier.allFeatures
    } reduce { (x, y) => x union y }
  }
}

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

