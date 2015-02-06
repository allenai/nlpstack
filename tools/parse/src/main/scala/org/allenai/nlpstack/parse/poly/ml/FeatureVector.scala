package org.allenai.nlpstack.parse.poly.ml

import spray.json.DefaultJsonProtocol._

/** The name of a feature, represented as a list of Symbols.
  *
  * @param symbols the list of symbols comprising the feature name
  */
case class FeatureName(symbols: List[Symbol]) {
  override def toString(): String = {
    (symbols map { sym => sym.name }).mkString(".")
  }
}

object FeatureName {
  implicit val jsFormat = jsonFormat1(FeatureName.apply)
}

/** A mapping from feature names to values.
  *
  * Unspecified feature names are assumed to correspond to a value of zero.
  *
  * @param values the map from feature names to values
  */
case class FeatureVector(values: Seq[(FeatureName, Double)]) {

  @transient lazy val featureNames = values map { _._1 }

  @transient lazy val featureMap = values.toMap

  /** Returns the value of the specified feature name.
    *
    * Note that this returns zero if the feature is not present in the map.
    *
    * @param name the feature name of interest
    * @return the value assigned to that feature name
    */
  def getFeatureValue(name: FeatureName): Double = {
    featureMap.getOrElse(name, 0.0)
  }

  override def toString(): String = {
    "[" + (values map {
      case (featureName, featureValue) =>
        f"${featureName} -> $featureValue%.3f"
    }).mkString(" ") + "]"
  }
}

object FeatureVector {
  implicit val jsFormat = jsonFormat1(FeatureVector.apply)

  /** Takes the difference between two feature vectors.
    *
    * @param vec1 first vector
    * @param vec2 second vector
    * @return the difference vector (first - second)
    */
  def subtractVectors(vec1: FeatureVector, vec2: FeatureVector): FeatureVector = {
    FeatureVector(((vec1.featureNames ++ vec2.featureNames) map { featureName =>
      (featureName, vec1.getFeatureValue(featureName)
        - vec2.getFeatureValue(featureName))
    }).toMap.toSeq)
  }

  /** Merges two feature vectors.
    *
    * In case of conflict, values in the first vector are preferred.
    *
    * @param vec1 first vector
    * @param vec2 second vector
    * @return the merged vector
    */
  def mergeVectors(vec1: FeatureVector, vec2: FeatureVector): FeatureVector = {
    FeatureVector((vec2.values.toMap ++ vec1.values.toMap).toSeq)
  }

}
