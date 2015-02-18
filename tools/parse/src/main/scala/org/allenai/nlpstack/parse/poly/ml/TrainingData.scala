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

/** Abstraction for a set of labeled feature vectors.
  *
  * Provides various serialization options for different machine learning tools.
  *
  * @param labeledVectors a sequence of feature vectors labeled with doubles
  */
case class TrainingData(labeledVectors: Iterable[(FeatureVector, Int)]) {

  /** The set of feature names found in the training data. */
  lazy val featureNames: Set[FeatureName] = {
    val featureNameSets: Iterable[Set[FeatureName]] = (labeledVectors map {
      case (fvec, _) =>
        fvec.featureNames.toSet
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
      case (fvec: FeatureVector, label) =>
        val sortedValues: Seq[(Int, Double)] = (fvec.values.toSeq map {
          case (featureName, featureValue) =>
            (signature.featureNameToIndex(featureName), featureValue)
        }).sortBy(_._1)
        val featureString = (sortedValues map {
          case (featureIndex, featureValue) =>
            s"${featureIndex}:${featureValue}"
        }).mkString(" ")
        s"${svmLightLabel(label)} ${featureString}"
    }).mkString("\n")
  }

  protected def svmLightLabel(label: Double): String = s"${label}"
}
