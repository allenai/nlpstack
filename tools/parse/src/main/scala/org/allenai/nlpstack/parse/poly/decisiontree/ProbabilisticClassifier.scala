package org.allenai.nlpstack.parse.poly.decisiontree

import reming.{ JsonFormat, JsonParser, JsonPrinter, LazyFormat }
import reming.DefaultJsonProtocol._

trait ProbabilisticClassifier {

  /** Gets the probability distribution over outcomes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return probability distribution of outcomes according to training data
    */
  def outcomeDistribution(featureVector: FeatureVector): Map[Int, Float]

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

/** Trait to be implemented based on the required structure for the justification for a particular
  * classifier.
  */
trait Justification

/** Probabilistic Classifier that classifies a given feature vector and provides justification for
  * the obtained outcome.
  */
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
    val (bestClass, (bestProb, bestClassJustification)) =
      outcomeDistributionWithJustification(featureVector) maxBy {
        case (_, (prob, justification)) => prob
      }
    (bestClass, bestClassJustification)
  }
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

/** Companion object to the JustifyingProbabilisticClassifier to enable proper deserialization.
  */
object JustifyingProbabilisticClassifier {
  implicit object JustifyingProbabilisticClassifierJsonFormat
      extends JsonFormat[JustifyingProbabilisticClassifier] {
    private val wrappedFormat = implicitly[LazyFormat[ProbabilisticClassifier]]
    override def write(obj: JustifyingProbabilisticClassifier, printer: JsonPrinter): Unit = {
      wrappedFormat.write(obj, printer)
    }
    override def read(parser: JsonParser): JustifyingProbabilisticClassifier = {
      wrappedFormat.read(parser) match {
        case j: JustifyingProbabilisticClassifier => j
        case bad => deserializationError("Non-JustifyingProbabilisticClassifier found: " + bad)
      }
    }
  }
}

trait ProbabilisticClassifierTrainer extends (FeatureVectorSource => ProbabilisticClassifier)

trait JustifyingProbabilisticClassifierTrainer extends (FeatureVectorSource => JustifyingProbabilisticClassifier)
