package org.allenai.nlpstack.parse.poly.decisiontree

case class OneVersusAll(binaryClassifiers: Seq[(Int, ProbabilisticClassifier)])
    extends ProbabilisticClassifier {

  override def distributionForInstance(inst: FeatureVector): Map[Int, Double] = {
    val unnormalizedDist: Seq[(Int, Double)] =
      binaryClassifiers map {
        case (category, classifier) =>
          (category, classifier.distributionForInstance(inst).getOrElse(1, 0.0000001))
      }
    ProbabilisticClassifier.normalizeDistribution(unnormalizedDist).toMap
  }

  override def allAttributes: Set[Int] = {
    binaryClassifiers map {
      case (_, classifier) =>
        classifier.allAttributes
    } reduce { (x, y) => x union y }
  }
}

class OneVersusAllTrainer(baseTrainer: ProbabilisticClassifierTrainer)
    extends ProbabilisticClassifierTrainer {

  override def apply(data: FeatureVectors): ProbabilisticClassifier = {
    val binaryClassifiers = data.allLabels.toSet.toSeq map { category: Int =>
      val mappedVectors = data.featureVectors map { featureVector =>
        featureVector.relabel(
          featureVector.label match {
            case Some(x) if x == category => 1
            case _ => 0
          }
        )
      }
      (category, baseTrainer(FeatureVectors(mappedVectors)))
    }
    OneVersusAll(binaryClassifiers)
  }
}

