package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.json._
import spray.json.DefaultJsonProtocol._
import spray.json._

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

  /** Boilerplate code to serialize a ProbabilisticClassifier to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM ProbabilisticClassifier, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object ProbabilisticClassifierJsonFormat
      extends RootJsonFormat[ProbabilisticClassifier] {

    implicit val decisionTreeFormat =
      jsonFormat4(DecisionTree.apply).pack("type" -> "DecisionTree")
    implicit val randomForestFormat =
      jsonFormat2(RandomForest.apply).pack(
        "type" -> "RandomForest"
      )
    implicit val perceptronFormat =
      jsonFormat2(Perceptron.apply).pack("type" -> "Perceptron")
    implicit val oneVersusAllFormat =
      jsonFormat1(OneVersusAll.apply).pack("type" -> "OneVersusAll")

    def write(classifier: ProbabilisticClassifier): JsValue = classifier match {
      case decisionTree: DecisionTree => decisionTree.toJson
      case randomForest: RandomForest => randomForest.toJson
      case perceptron: Perceptron => perceptron.toJson
      case oneVersusAll: OneVersusAll => oneVersusAll.toJson
      case x => deserializationError(s"Cannot serialize this classifier type: $x")
    }

    def read(value: JsValue): ProbabilisticClassifier = value.asJsObject.unpackWith(
      decisionTreeFormat, randomForestFormat, perceptronFormat, oneVersusAllFormat
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

trait ProbabilisticClassifierTrainer extends (FeatureVectors => ProbabilisticClassifier)
