package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.json._
import spray.json.DefaultJsonProtocol._
import spray.json._

trait ProbabilisticClassifier {

  /** Gets the probability distribution over labels.
    *
    * @param inst instance to find distribution of
    * @return probability distribution of label according to training data
    */
  def distributionForInstance(inst: FeatureVector): Map[Int, Double]

  /** Classifies an instance.
    *
    * @param inst instance to classify
    * @return predicted label
    */
  def classify(inst: FeatureVector): Int = {
    val (bestClass, bestProb) = distributionForInstance(inst) maxBy { case (_, prob) => prob }
    bestClass
  }

  /** All attributes used by the classifier. */
  def allAttributes: Set[Int]
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
    //implicit val randomForestFormat =
    //  jsonFormat2(RandomForest.apply).pack(
    //    "type" -> "RandomForest"
    //  )
    //implicit val oneVersusAllFormat =
    //  jsonFormat1(OneVersusAll.apply).pack("type" -> "OneVersusAll")

    def write(classifier: ProbabilisticClassifier): JsValue = classifier match {
      case decisionTree: DecisionTree => decisionTree.toJson
      //case randomForest: RandomForest => randomForest.toJson
      //case oneVersusAll: OneVersusAll => oneVersusAll.toJson
      case x => deserializationError(s"Cannot serialize this classifier type: $x")
    }

    def read(value: JsValue): ProbabilisticClassifier = value.asJsObject.unpackWith(
      decisionTreeFormat //, randomForestFormat, oneVersusAllFormat
    )
  }

  def normalizeDistribution(unnormalizedDist: Seq[(Int, Double)]): Seq[(Int, Double)] = {
    val normalizer: Double = (unnormalizedDist map { _._2 }).sum
    require(normalizer > 0d)
    unnormalizedDist map {
      case (category, unnormalized) =>
        (category, unnormalized / normalizer)
    }
  }

  def addMaps(m1: Map[Int, Int], m2: Map[Int, Int]): Map[Int, Int] = {
    ((m1.keys ++ m2.keys).toSet map { key: Int =>
      (key, m1.getOrElse(key, 0) + m2.getOrElse(key, 0))
    }).toMap
  }
}

trait ProbabilisticClassifierTrainer extends (FeatureVectors => ProbabilisticClassifier)
