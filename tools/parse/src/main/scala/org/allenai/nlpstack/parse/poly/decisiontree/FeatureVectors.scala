package org.allenai.nlpstack.parse.poly.decisiontree

import spray.json.DefaultJsonProtocol._

/** FeatureVectors is a convenience container for feature vectors.
  *
  * The number of attributes must be the same for all feature vectors in the container.
  *
  * @param featureVectors collection of FeatureVector objects
  */
case class FeatureVectors(featureVectors: IndexedSeq[FeatureVector]) {

  // The number of attributes must be the same for all feature vectors.
  require((featureVectors map { _.numAttributes }).toSet.size <= 1,
    "the number of attributes must be the same for all feature vectors")

  /** Gets the number of feature vectors.
    *
    * @return the number of feature vectors
    */
  def numVectors: Int = featureVectors.size

  /** Gets the number of attributes.
    *
    * @return the number of attributes
    */
  def numAttributes: Int = {
    featureVectors.headOption map { _.numAttributes } getOrElse { 0 }
  }

  @transient lazy val allLabels: Seq[Int] = {
    (featureVectors flatMap { fv => fv.label }).toSet.toSeq
  }
}

private object FeatureVectors {
  implicit val jsFormat = jsonFormat1(FeatureVectors.apply)
}
