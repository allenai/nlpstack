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

/** A feature vector with integral features and label. */
sealed trait FeatureVector {

  /** label of instance */
  def label: Option[Int]

  /** number of attributes */
  def numAttributes: Int

  def nonzeroAttributes: Iterator[Int]

  /** gets value of attribute
    *
    * @param index attribute index
    * @return attribute value
    */
  def getAttribute(index: Int): Int

  def relabel(label: Int): FeatureVector
}

/** Instance with sparse binary attributes
  *
  * @param label label of instance
  * @param numAttributes number of attributes
  * @param trueAttributes which attributes have value 1
  */
case class SparseVector(override val label: Option[Int], override val numAttributes: Int,
    trueAttributes: Set[Int]) extends FeatureVector {

  override def getAttribute(i: Int): Int = {
    require(i < numAttributes)
    if (trueAttributes.contains(i)) {
      1
    } else {
      0
    }
  }

  override def nonzeroAttributes: Iterator[Int] = {
    trueAttributes.iterator
  }

  override def relabel(newLabel: Int): FeatureVector = {
    copy(label = Some(newLabel))
  }
}

/** Instance with arbitrary integral attributes
  *
  * @param label label of instance
  * @param attributes value of each attribute
  */
case class DenseVector(
    override val label: Option[Int],
    attributes: IndexedSeq[Int]
) extends FeatureVector {

  def numAttributes: Int = attributes.size

  override def getAttribute(i: Int): Int = {
    require(i < numAttributes)
    attributes(i)
  }

  override def nonzeroAttributes: Iterator[Int] = ???

  override def relabel(newLabel: Int): FeatureVector = {
    copy(label = Some(newLabel))
  }
}
