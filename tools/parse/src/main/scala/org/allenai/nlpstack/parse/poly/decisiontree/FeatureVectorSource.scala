package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.nlpstack.parse.poly.fsm.ClassificationTask
import spray.json.DefaultJsonProtocol._

trait FeatureVectorSource {
  def vectorIterator: Iterator[FeatureVector]

  def getNthVector(n: Int): FeatureVector

  /** Gets the number of feature vectors in this collection.
    *
    * @return the number of feature vectors in this collection
    */
  def numVectors: Int

  /** Gets the number of features in this collection.
    *
    * @return the number of features in this collection
    */
  def numFeatures: Int

  /** Gets the set of features in this collection of feature vectors.
    *
    * @return the set of features that appear in at least one vector of this collection
    */
  def getFeatures: Set[Int]

  /** Gets a uniqued sequence of all outcomes associated with feature vectors in this set. */
  val allOutcomes: Seq[Int]

  val classificationTask: ClassificationTask
}

/** FeatureVectors is a convenience container for feature vectors.
  *
  * The number of features must be the same for all feature vectors in the container.
  *
  * @param featureVecs collection of FeatureVector objects
  */
case class InMemoryFeatureVectorSource(
    featureVecs: IndexedSeq[FeatureVector], classificationTask: ClassificationTask
) extends FeatureVectorSource {

  // The number of features must be the same for all feature vectors.
  require(
    (featureVecs map { _.numFeatures }).toSet.size <= 1,
    "the number of features must be the same for all feature vectors"
  )

  def vectorIterator: Iterator[FeatureVector] = featureVecs.iterator

  def getNthVector(n: Int): FeatureVector = featureVecs(n)

  /** Gets the number of feature vectors in this collection.
    *
    * @return the number of feature vectors in this collection
    */
  def numVectors: Int = featureVecs.size

  /** Gets the number of features in this collection.
    *
    * @return the number of features in this collection
    */
  def numFeatures: Int = {
    getFeatures.size
  }

  /** Gets the set of features in this collection of feature vectors.
    *
    * @return the set of features that appear in at least one vector of this collection
    */
  def getFeatures: Set[Int] = {
    featureVecs.foldLeft(Set[Int]()) { (y, fvec) => y ++ fvec.nonzeroFeatures }
  }

  /** Gets a uniqued sequence of all outcomes associated with feature vectors in this set. */
  @transient lazy val allOutcomes: Seq[Int] = {
    (featureVecs flatMap { fv => fv.outcome }).toSet.toSeq
  }
}

private object InMemoryFeatureVectorSource {
  implicit val jsFormat = jsonFormat2(InMemoryFeatureVectorSource.apply)
}

case class RemappedFeatureVectorSource(
    fvSource: FeatureVectorSource,
    outcomeRemapping: Int => Int
) extends FeatureVectorSource {

  override def vectorIterator: Iterator[FeatureVector] = {
    fvSource.vectorIterator map { vec => remapOutcome(vec) }
  }

  override def getNthVector(n: Int): FeatureVector = {
    remapOutcome(fvSource.getNthVector(n))
  }

  private def remapOutcome(vec: FeatureVector) = vec.outcome match {
    case Some(outcome) =>
      vec.modifyOutcome(outcomeRemapping(outcome))
    case None =>
      vec
  }

  override val classificationTask: ClassificationTask = fvSource.classificationTask

  /** Gets the number of feature vectors in this collection.
    *
    * @return the number of feature vectors in this collection
    */
  override def numVectors: Int = fvSource.numVectors

  /** Gets the set of features in this collection of feature vectors.
    *
    * @return the set of features that appear in at least one vector of this collection
    */
  override def numFeatures: Int = fvSource.numFeatures

  override def getFeatures: Set[Int] = fvSource.getFeatures

  override val allOutcomes: Seq[Int] = (fvSource.allOutcomes map { outcome =>
    outcomeRemapping(outcome)
  }).toSet.toSeq
}
