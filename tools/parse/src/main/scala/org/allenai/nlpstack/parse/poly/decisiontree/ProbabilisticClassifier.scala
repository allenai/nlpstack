package org.allenai.nlpstack.parse.poly.decisiontree

import reming.LazyFormat
import reming.DefaultJsonProtocol._

trait ProbabilisticClassifier {

  /** Gets the probability distribution over outcomes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return probability distribution of outcomes according to training data
    */
  def outcomeDistribution(featureVector: FeatureVector): Map[Int, Double]

  /** Classifies an feature vector.
    *
    * @param featureVector feature vector to classify
    * @return predicted outcome
    */
  def classify(featureVector: FeatureVector): Int = {
    val (bestClass, bestProb) = outcomeDistribution(featureVector) maxBy { case (_, prob) => prob }
    bestClass
  }

  /** All features used by the classifier. */
  def allFeatures: Set[Int]
}

object ProbabilisticClassifier {

  implicit object ProbabilisticClassifierFormat extends LazyFormat[ProbabilisticClassifier] {
    private implicit val randomForestFormat = jsonFormat2(RandomForest.apply)
    private implicit val oneVersusAllFormat = jsonFormat1(OneVersusAll.apply)

    override val delegate = parentFormat[ProbabilisticClassifier](
      childFormat[DecisionTree, ProbabilisticClassifier],
      childFormat[RandomForest, ProbabilisticClassifier],
      childFormat[OneVersusAll, ProbabilisticClassifier]
    )
  }

  /** Normalizes an unnormalized distribution over integers.
    *
    * If the sum of the original masses is zero, then this will return a uniform distribution
    * over the domain.
    *
    * @param unnormalizedDist a map from integers to probability mass (not necessarily normalized)
    * @return the normalized version of the argument distribution
    */
  def normalizeDistribution(unnormalizedDist: Seq[(Int, Double)]): Seq[(Int, Double)] = {
    require(unnormalizedDist.nonEmpty, ".normalizeDistribution cannot be called on an empty seq")
    val normalizer: Double = (unnormalizedDist map { _._2 }).sum
    if (normalizer > 0d) {
      unnormalizedDist map {
        case (outcome, unnormalized) =>
          (outcome, unnormalized / normalizer)
      }
    } else {
      unnormalizedDist map {
        case (outcome, _) =>
          (outcome, 1.0 / unnormalizedDist.length)
      }
    }
  }

  def addMaps(m1: Map[Int, Int], m2: Map[Int, Int]): Map[Int, Int] = {
    ((m1.keys ++ m2.keys).toSet map { key: Int =>
      (key, m1.getOrElse(key, 0) + m2.getOrElse(key, 0))
    }).toMap
  }
}

trait ProbabilisticClassifierTrainer extends (FeatureVectorSource => ProbabilisticClassifier)
