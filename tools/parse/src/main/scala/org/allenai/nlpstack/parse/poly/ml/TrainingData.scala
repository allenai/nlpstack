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
case class TrainingData(labeledVectors: Iterable[(FeatureVector, Double)]) {

  /** The set of feature names found in the training data. */
  lazy val featureNames: Set[FeatureName] = {
    val featureNameSets: Iterable[Set[FeatureName]] = (labeledVectors map {
      case (fvec, _) =>
        fvec.featureNames.toSet
    })
    featureNameSets.fold(Set[FeatureName]())((x: Set[FeatureName], y: Set[FeatureName]) =>
      x union y)
  }

  /** Creates "positive" and "negative" feature vectors according to whether the feature
    * cost is greater than `margin` or less than -`margin`, respectively.
    *
    * Feature vectors that are within `margin` of zero are filtered from the traing data.
    *
    * @param margin the absolute threshold that determines whether a vector is kept
    * @return a TrainingData instance where all costs are -1 or 1
    */
  def binarize(margin: Double): BinaryTrainingData = {
    new BinaryTrainingData(labeledVectors filter {
      case (_, label) =>
        label.abs >= margin
    } map {
      case (vec, label) =>
        (vec, math.signum(label))
    })
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

/** A subinstance of TrainingData whose labels are -1 or 1.
  *
  * @param labeledVectors a sequence of feature vectors labeled with doubles
  */
class BinaryTrainingData(
  override val labeledVectors: Iterable[(FeatureVector, Double)]
)
    extends TrainingData(labeledVectors) {

  override def svmLightLabel(label: Double): String = {
    label match {
      case x if x < 0 => "-1"
      case _ => "+1"
    }
  }
}
