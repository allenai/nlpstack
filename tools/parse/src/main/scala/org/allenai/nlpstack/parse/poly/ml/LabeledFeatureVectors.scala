package org.allenai.nlpstack.parse.poly.ml

import spray.json.DefaultJsonProtocol._

/** Maps feature names to integers. Useful for serializing TrainingData instances for
  * consumption by command-line machine learning tools.
  *
  * @param featureNames an indexed sequence of feature names
  */
case class FeatureEncoding(featureNames: IndexedSeq[FeatureName]) {
  @transient lazy val featureNameToIndex: Map[FeatureName, Int] = featureNames.zipWithIndex.toMap
}

object FeatureEncoding {
  implicit val jsFormat = jsonFormat1(FeatureEncoding.apply)
}

/** Support structure to store a feature vector per parse family with associated label for expected
  * outcome.
  */
case class LabeledFeatureVector(featureVector: FeatureVector, expectedOutcome: Int) {
}

/** Abstraction for a set of labeled feature vectors.
  *
  * Provides various serialization options for different machine learning tools.
  *
  * @param labeledVectors a sequence of families and their feature vectors labeled with
  * integer outcomes
  */
case class LabeledFeatureVectors(
    labeledVectors: Iterable[LabeledFeatureVector]
) {

  /** The set of feature names found in the training data. */
  lazy val featureNames: Set[FeatureName] = {
    val featureNameSets: Iterable[Set[FeatureName]] = (labeledVectors map {
      x => x.featureVector.featureNames.toSet
    })
    featureNameSets.fold(Set[FeatureName]())((x: Set[FeatureName], y: Set[FeatureName]) =>
      x union y)
  }

  /** Expresses this training data in "SVMlight" format, which is
    * <line> .=. <target> <feature>:<value> ... <feature>:<value> # <info>
    * <target> .=. +1 | -1 | 0 | <float>
    * <feature> .=. <integer> | "qid"
    * <value> .=. <float>
    * <info> .=. <string>
    *
    * @param signature the signature to use for encoding feature names as integer
    * @return the training data in SVMlight format
    */
  def asSvmLight(signature: FeatureEncoding): String = {
    (labeledVectors map {
      x =>
        val sortedValues: Seq[(Int, Double)] = (x.featureVector.values.toSeq map {
          case (featureName, featureValue) =>
            (signature.featureNameToIndex(featureName), featureValue)
        }).sortBy(_._1)
        val featureString = (sortedValues map {
          case (featureIndex, featureValue) =>
            s"${featureIndex}:${featureValue}"
        }).mkString(" ")
        s"${svmLightLabel(x.expectedOutcome)} ${featureString}"
    }).mkString("\n")
  }

  protected def svmLightLabel(label: Double): String = s"${label}"
}
