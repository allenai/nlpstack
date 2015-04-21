package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.nlpstack.parse.poly.ml.FeatureName
import reming.{ JsonFormat, JsonParser, JsonPrinter, LazyFormat }
import reming.DefaultJsonProtocol._

/* Abstract "justification" for a particular classifier decision. */
trait Justification {
  def prettyPrint(featureNames: Map[Int, FeatureName])
}

trait ProbabilisticClassifier {

  /** Gets the probability distribution over outcomes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return probability distribution of outcomes according to training data
    */
  def outcomeDistribution(featureVector: FeatureVector): Map[Int, Float]

  /** Classifies an feature vector and optionally returns a "justification" for the classification
    * decision.
    *
    * @param featureVector feature vector to classify
    * @return (predicted outcome, optional justification for the prediction)
    */
  def classify(featureVector: FeatureVector): (Int, Option[Justification]) //= {
  //val (bestClass, _) = outcomeDistribution(featureVector) maxBy { case (_, prob) => prob }
  //bestClass
  //}

  /** All features used by the classifier. */
  def allFeatures: Set[Int]
}

/** Probabilistic Classifier that classifies a given feature vector and provides justification for
  * the obtained outcome.
  */
/*
trait JustifyingProbabilisticClassifier extends ProbabilisticClassifier {

  /** Gets the probability distribution over outcomes, with justification for each outcome in the
    * distribution.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return probability distribution of outcomes according to training data with associated
    * explanations for each outcome.
    */
  def outcomeDistributionWithJustification(
    featureVector: FeatureVector
  ): Map[Int, (Float, Justification)]

  /** Classifies a feature vector and produces a justification for the result produced.
    *
    * @param featureVector feature vector to classify
    * @return predicted outcome with justification
    */
  def classifyAndJustify(featureVector: FeatureVector): (Int, Justification) = {
    val (bestClass, (bestProb, bestClassJustification)) = {
      val s = outcomeDistributionWithJustification(featureVector)
      println(s)
      s maxBy {
        case (_, (prob, justification)) => prob
      }
    }
    println(bestClass)
    (bestClass, bestClassJustification)
  }
}
*/

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
  def normalizeDistribution(unnormalizedDist: Seq[(Int, Float)]): Seq[(Int, Float)] = {
    require(unnormalizedDist.nonEmpty, ".normalizeDistribution cannot be called on an empty seq")
    val normalizer: Double = (unnormalizedDist map { _._2 }).sum
    if (normalizer > 0f) {
      unnormalizedDist map {
        case (outcome, unnormalized) =>
          (outcome, unnormalized / normalizer.toFloat)
      }
    } else {
      unnormalizedDist map {
        case (outcome, _) =>
          (outcome, 1.0f / unnormalizedDist.length)
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
