package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }
import spray.json.DefaultJsonProtocol._
import spray.json._
import org.allenai.common.json._

/** Maps a selected node of a parse tree into a feature vector. */
abstract class ParseNodeFeature extends ((PolytreeParse, Int) => FeatureVector)

object ParseNodeFeature {

  /** Boilerplate code to serialize a ParseNodeFeature to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM ParseNodeFeature, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object ParseNodeFeatureJsonFormat extends RootJsonFormat[ParseNodeFeature] {

    implicit val parseNodeFeatureUnionFormat =
      jsonFormat1(ParseNodeFeatureUnion.apply).pack("type" -> "ParseNodeFeatureUnion")

    implicit val transformedNeighborhoodFeatureFormat =
      jsonFormat2(TransformedNeighborhoodFeature.apply).pack(
        "type" -> "TransformedNeighborhoodFeature"
      )

    def write(feature: ParseNodeFeature): JsValue = feature match {
      case ChildrenArcLabelsFeature => JsString("ChildrenArcLabelsFeature")
      case NumChildren => JsString("NumChildren")
      case parseNodeFeatureUnion: ParseNodeFeatureUnion =>
        parseNodeFeatureUnion.toJson
      case transformedNeighborhoodFeature: TransformedNeighborhoodFeature =>
        transformedNeighborhoodFeature.toJson
    }

    def read(value: JsValue): ParseNodeFeature = value match {
      case JsString(typeid) => typeid match {
        case "ChildrenArcLabelsFeature" => ChildrenArcLabelsFeature
        case "NumChildren" => NumChildren
        case x => deserializationError(s"Invalid identifier for TaskIdentifier: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        parseNodeFeatureUnionFormat,
        transformedNeighborhoodFeatureFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** A ParseNodeFeatureUnion merges the output of a list of features.
  *
  * @param features a list of the features we want to merge into a single feature
  */
case class ParseNodeFeatureUnion(
    features: Seq[ParseNodeFeature]
) extends ParseNodeFeature {

  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    features map (f => f(parse, token)) reduce ((m1, m2) => FeatureVector.mergeVectors(m1, m2))
  }
}

case class TransformedNeighborhoodFeature(
    neighborhoodExtractors: Seq[(String, NeighborhoodExtractor)],
    transforms: Seq[(String, NeighborhoodTransform)]
) extends ParseNodeFeature {

  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    FeatureVector(
      for {
        (extractorName, extractor) <- neighborhoodExtractors
        neighborhood <- extractor(parse, token)
        (transformName, transform) <- transforms
        transformedNeighborhood <- transform(parse, neighborhood)
      } yield {
        val featureName = (Seq(extractorName, transformName) map { x => Symbol(x) }) ++
          transformedNeighborhood.symbols
        FeatureName(featureName) -> 1.0
      }
    )
  }
}

case object ChildrenArcLabelsFeature extends ParseNodeFeature {
  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    FeatureVector(Seq(
      FeatureName('childArcLabels +: (parse.families(token).tail.toList map { tok =>
        parse.arcLabelByEndNodes(Set(tok, token))
      })) -> 1.0
    ))
  }
}

case object NumChildren extends ParseNodeFeature {
  override def apply(parse: PolytreeParse, token: Int): FeatureVector = {
    FeatureVector(
      (FeatureName(Seq('numChildren, Symbol(parse.children(token).size.toString))) -> 1.0) +:
        (Range(0, 1 + parse.children(token).size) map { numChildren =>
          FeatureName(Seq('numChildren, Symbol(s"$numChildren+"))) -> 1.0
        })
    )
  }
}
