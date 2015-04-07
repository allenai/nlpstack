package org.allenai.nlpstack.parse.poly.ml

import org.allenai.nlpstack.parse.poly.core.Util

import reming.DefaultJsonProtocol._

/** A weighted linear combination of features.
  *
  * @param coefficients map from feature names to weight coefficients
  */
case class LinearModel(val coefficients: Seq[(FeatureName, Double)]) {

  @transient val coefficientMap = coefficients.toMap

  /** Returns the coefficient corresponding to the specified feature name.
    *
    * For unspecified coefficients, zero is returned.
    *
    * @param featureName the feature name of interest
    * @return the coefficient corresponding to the specified feature name
    */
  def getCoefficient(featureName: FeatureName): Double = {
    coefficientMap.getOrElse(featureName, 0.0)
  }

  /** Computes the weighted linear combination, given the feature values in the argument vector.
    *
    * @param featureVector the feature vector of interest
    * @return the weighted linear combination
    */
  def score(featureVector: FeatureVector): Double = {
    def add(x: Double, y: Double): Double = { x + y }
    (featureVector.featureNames map { featureName =>
      getCoefficient(featureName) * featureVector.getFeatureValue(featureName)
    }).fold(0.0)(add)
  }
}

object LinearModel {
  implicit val jsFormat = jsonFormat1(LinearModel.apply)

  def loadLinearModel(filename: String): LinearModel = Util.readFromFile[LinearModel](filename)
}
