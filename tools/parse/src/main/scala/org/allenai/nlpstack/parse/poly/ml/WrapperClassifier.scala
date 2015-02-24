package org.allenai.nlpstack.parse.poly.ml

import org.allenai.nlpstack.parse.poly.decisiontree.{
  FeatureVector => DTFeatureVector,
  ProbabilisticClassifierTrainer,
  InMemoryFeatureVectorSource,
  SparseVector,
  ProbabilisticClassifier
}
import org.allenai.nlpstack.parse.poly.fsm.SimpleTask
import spray.json.DefaultJsonProtocol._
import scala.collection.immutable.HashSet

/** A WrapperClassifier wraps a ProbabilisticClassifier (which uses integer-based feature
  * names) in an interface that allows you to use the more natural
  * org.allenai.nlpstack.parse.poly.ml FeatureVector format for classification.
  *
  * @param classifier the embedded classifier (which uses integer-based feature names)
  * @param featureNameMap a map from the integer feature names to their string-based form
  */
case class WrapperClassifier(
    classifier: ProbabilisticClassifier,
    featureNameMap: Seq[(Int, FeatureName)]
) {

  /** The inverse of featureNameMap. */
  @transient
  private val featureNameToIndex: Map[FeatureName, Int] =
    (featureNameMap map {
      case (featIndex, feat) =>
        (feat, featIndex)
    }).toMap

  /** Returns the most probable (integer) outcome, given the input feature vector.
    *
    * @param featureVector the feature vector to classify
    * @return the most probable (integer) outcome
    */
  def classify(featureVector: FeatureVector): Int = {
    classifier.classify(
      WrapperClassifier.createDTFeatureVector(featureVector, featureNameToIndex, None)
    )
  }

  /** Returns a distributions over all (integer) outcomes, given the input feature vector.
    *
    * @param featureVector the feature vector to classify
    * @return the most probable (integer) outcome
    */
  def getDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    classifier.outcomeDistribution(
      WrapperClassifier.createDTFeatureVector(featureVector, featureNameToIndex, None)
    )
  }
}

object WrapperClassifier {
  implicit val wcFormat = jsonFormat2(WrapperClassifier.apply)

  /** Converts a string-based feature vector into an integer-based feature vector.
    *
    * @param featureVector the string-based feature vector
    * @param featureNameToIndex a map from string-based feature names to integers
    * @param outcome the outcome (if known) of the feature vector
    * @return the equivalent integer-based feature vector
    */
  def createDTFeatureVector(
    featureVector: FeatureVector,
    featureNameToIndex: Map[FeatureName, Int],
    outcome: Option[Int]
  ): DTFeatureVector = {
    val trueAttributeNames: Seq[FeatureName] =
      featureVector.values filter { _._2 != 0 } map { _._1 }
    val trueAttributes: HashSet[Int] =
      HashSet(trueAttributeNames
        .filter(featureNameToIndex.contains)
        .map(featureNameToIndex).toSeq: _*)
    new SparseVector(outcome, featureNameToIndex.values.max + 1, trueAttributes)
  }
}

/** Trains a WrapperClassifier from training data.
  *
  * @param classifierTrainer the underlying trainer to use (from the decisiontree package)
  */
class WrapperClassifierTrainer(classifierTrainer: ProbabilisticClassifierTrainer) {

  def apply(trainingData: TrainingData): WrapperClassifier = {
    val featureNames: Seq[FeatureName] = trainingData.featureNames.toSeq
    val featureNameToIndex: Map[FeatureName, Int] = featureNames.zipWithIndex.toMap
    val vectors = new InMemoryFeatureVectorSource(
      (trainingData.labeledVectors map {
      case (vec, outcome) =>
        WrapperClassifier.createDTFeatureVector(vec, featureNameToIndex, Some(outcome))
    }).toIndexedSeq,
      SimpleTask("basic")
    )
    println(s"Task has ${vectors.numVectors} training vectors")
    val inducedClassifier: ProbabilisticClassifier = classifierTrainer(vectors)
    val featureMap: Seq[(Int, FeatureName)] =
      featureNames.zipWithIndex filter {
        case (_, featIndex) =>
          inducedClassifier.allFeatures.contains(featIndex)
      } map {
        case (feat, featIndex) =>
          (featIndex, feat)
      }
    WrapperClassifier(inducedClassifier, featureMap)
  }
}
