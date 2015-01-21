package org.allenai.nlpstack.parse.poly.decisiontree

import spray.json.DefaultJsonProtocol._
import scala.util.Random

/** FeatureVectors is a convenience container for feature vectors.
  *
  * The number of features must be the same for all feature vectors in the container.
  *
  * @param featureVectors collection of FeatureVector objects
  */
case class FeatureVectors(featureVectors: IndexedSeq[FeatureVector]) {

  // The number of features must be the same for all feature vectors.
  require(
    (featureVectors map { _.numFeatures }).toSet.size <= 1,
    "the number of features must be the same for all feature vectors"
  )

  /** Gets the number of feature vectors in this collection.
    *
    * @return the number of feature vectors in this collection
    */
  def numVectors: Int = featureVectors.size

  /** Gets the number of features in this collection.
    *
    * @return the number of features in this collection
    */
  def numFeatures: Int = {
    featureVectors.headOption map { _.numFeatures } getOrElse { 0 }
  }

  /** Gets a uniqued sequence of all outcomes associated with feature vectors in this set. */
  @transient lazy val allOutcomes: Seq[Int] = {
    (featureVectors flatMap { fv => fv.outcome }).toSet.toSeq
  }

  /** Returns a "bag", i.e. N samples (with replacement) from this set of feature vectors,
    * where N is the original size of this feature vector set.
    *
    * @return a randomly sampled bag
    */
  def getBag: FeatureVectors = {
    val bag = Range(0, this.numVectors) map { x =>
      Random.nextInt(this.numVectors)
    }
    FeatureVectors(bag map featureVectors)
  }

}

private object FeatureVectors {
  implicit val jsFormat = jsonFormat1(FeatureVectors.apply)
}
