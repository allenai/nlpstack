package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.json._

import spray.json._
import spray.json.DefaultJsonProtocol._

private object FeatureVector {

  implicit val denseFormat = jsonFormat2(DenseVector.apply).pack("type" -> "dense")

  implicit val sparseFormat = jsonFormat3(SparseVector.apply).pack("type" -> "sparse")

  implicit object FeatureVectorJsonFormat extends JsonFormat[FeatureVector] {

    override def write(inst: FeatureVector): JsValue = inst match {
      case dense: DenseVector => dense.toJson
      case sparse: SparseVector => sparse.toJson
    }

    override def read(value: JsValue): FeatureVector = {
      value.asJsObject.unpackWith[FeatureVector](denseFormat, sparseFormat)
    }
  }
}

/** A feature vector with integral features and outcome. */
sealed trait FeatureVector {

  /** The outcome of this feature vector. */
  def outcome: Option[Int]

  /** Returns a copy of this feature vector, associated with a different outcome.
    *
    * @param outcome the new desired outcome
    * @return a copy of this feature vector, associated with a different outcome.
    */
  def modifyOutcome(outcome: Int): FeatureVector

  /** The number of features in this feature vector. */
  def numFeatures: Int

  /** Returns an iterator over all non-zero features in this feature vector. */
  def nonzeroFeatures: Iterator[Int]

  /** Gets the value of the specified feature.
    *
    * @param index the feature index
    * @return the feature value
    */
  def getFeature(index: Int): Int
}

/** A SparseVector is a feature vector with sparse binary features.
  *
  * @param outcome the outcome of the feature vector
  * @param numFeatures the number of features
  * @param trueFeatures the set of features with value 1
  */
case class SparseVector(override val outcome: Option[Int], override val numFeatures: Int,
    trueFeatures: Set[Int]) extends FeatureVector {

  override def getFeature(i: Int): Int = {
    require(i < numFeatures)
    if (trueFeatures.contains(i)) {
      1
    } else {
      0
    }
  }

  override def nonzeroFeatures: Iterator[Int] = {
    trueFeatures.iterator
  }

  override def modifyOutcome(newLabel: Int): FeatureVector = {
    copy(outcome = Some(newLabel))
  }
}

/** A DenseVector is a feature vector with arbitrary integral features.
  *
  * @param outcome the outcome of the feature vector
  * @param features the value of each feature
  */
case class DenseVector(
    override val outcome: Option[Int],
    features: IndexedSeq[Int]
) extends FeatureVector {

  def numFeatures: Int = features.size

  override def getFeature(i: Int): Int = {
    require(i < numFeatures)
    features(i)
  }

  override def nonzeroFeatures: Iterator[Int] = ???

  override def modifyOutcome(newLabel: Int): FeatureVector = {
    copy(outcome = Some(newLabel))
  }
}
