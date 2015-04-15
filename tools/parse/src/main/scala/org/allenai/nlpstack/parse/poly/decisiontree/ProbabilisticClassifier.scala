package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.json._
import spray.json._
import spray.json.DefaultJsonProtocol._

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

/** Trait to be implemented based on the required structure for the justification for a particular
  * classifier.
  */
trait Justification

/** Probabilistic Classifier that classifies a given feature vector and provides justification for
  * the obtained outcome.
  */
trait JustifyingProbabilisticClassifier extends ProbabilisticClassifier {

  /** Gets the probability distribution over outcomes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return probability distribution of outcomes according to training data with associated
    * explanations for each outcome.
    */
  def outcomeDistributionWithJustification(
    featureVector: FeatureVector
  ): Map[Int, (Double, Justification)]

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

  /** Boilerplate code to serialize a ProbabilisticClassifier to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM ProbabilisticClassifier, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object ProbabilisticClassifierJsonFormat
      extends RootJsonFormat[ProbabilisticClassifier] {

    implicit val decisionTreeFormat = DecisionTree.dtFormat.pack("type" -> "DecisionTree")
    implicit val randomForestFormat =
      jsonFormat2(RandomForest.apply).pack(
        "type" -> "RandomForest"
      )
    implicit val oneVersusAllFormat =
      jsonFormat1(OneVersusAll.apply).pack("type" -> "OneVersusAll")

    def write(classifier: ProbabilisticClassifier): JsValue = classifier match {
      case decisionTree: DecisionTree => decisionTree.toJson
      case randomForest: RandomForest => randomForest.toJson
      case oneVersusAll: OneVersusAll => oneVersusAll.toJson
      case x => deserializationError(s"Cannot serialize this classifier type: $x")
    }

    def read(value: JsValue): ProbabilisticClassifier = {
      value.asJsObject.unpackWith(decisionTreeFormat, randomForestFormat, oneVersusAllFormat)
    }
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

/** Companion object to the JustifyingProbabilisticClassifier to enable proper deserialization.
  */
object JustifyingProbabilisticClassifier {
  import ProbabilisticClassifier.ProbabilisticClassifierJsonFormat
  import ProbabilisticClassifier.ProbabilisticClassifierJsonFormat._

  implicit object JustifyingProbabilisticClassifierJsonFormat
      extends RootJsonFormat[JustifyingProbabilisticClassifier] {

    def write(classifier: JustifyingProbabilisticClassifier): JsValue = {
      ProbabilisticClassifierJsonFormat.write(classifier)
    }

    def read(value: JsValue): JustifyingProbabilisticClassifier = {
      ProbabilisticClassifierJsonFormat.read(value) match {
        case classifier: JustifyingProbabilisticClassifier => classifier
        case x => deserializationError(s"Cannot serialize this classifier type: $x")
      }
    }
  }
}

trait ProbabilisticClassifierTrainer extends (FeatureVectorSource => ProbabilisticClassifier)

trait JustifyingProbabilisticClassifierTrainer extends (FeatureVectorSource => JustifyingProbabilisticClassifier)
