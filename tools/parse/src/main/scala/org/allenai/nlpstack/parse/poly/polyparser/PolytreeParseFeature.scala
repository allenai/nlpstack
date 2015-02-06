package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.ml.{
  FeatureVector => MLFeatureVector,
  FeatureName => MLFeatureName
}
import org.allenai.common.json._
import spray.json.DefaultJsonProtocol._
import spray.json._

/** Maps a scored parse into a feature vector. */
abstract class PolytreeParseFeature extends ((PolytreeParse, Double) => MLFeatureVector)

object PolytreeParseFeature {

  /** Boilerplate code to serialize a PolytreeParseFeature to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM PolytreeParseFeature, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object PolytreeParseFeatureJsonFormat extends RootJsonFormat[PolytreeParseFeature] {

    implicit val eventStatisticFeaturesFormat =
      jsonFormat2(EventStatisticFeatures.apply).pack("type" -> "EventStatisticFeatures")
    implicit val polytreeParseFeatureUnionFormat =
      jsonFormat1(PolytreeParseFeatureUnion.apply).pack("type" -> "PolytreeParseFeatureUnion")

    def write(feature: PolytreeParseFeature): JsValue = feature match {
      case BaseParserScoreFeature => JsString("BaseParserScoreFeature")
      case SentenceLengthFeature => JsString("SentenceLengthFeature")
      case eventStatisticFeatures: EventStatisticFeatures =>
        eventStatisticFeatures.toJson
      case polytreeParseFeatureUnion: PolytreeParseFeatureUnion =>
        polytreeParseFeatureUnion.toJson
    }

    def read(value: JsValue): PolytreeParseFeature = value match {
      case JsString(typeid) => typeid match {
        case "BaseParserScoreFeature" => BaseParserScoreFeature
        case "SentenceLengthFeature" => SentenceLengthFeature
        case x => deserializationError(s"Invalid identifier for TaskIdentifier: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        eventStatisticFeaturesFormat,
        polytreeParseFeatureUnionFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** Generates a feature for each neighborhood histogram and transform in the argument list.
  *
  * @param neighborhoodCounts the neighborhood histograms
  * @param transforms the neighborhood transforms
  */
case class EventStatisticFeatures(
    neighborhoodCounts: Seq[(String, NeighborhoodExtractor, Seq[(Neighborhood, Int)])],
    transforms: Seq[(String, NeighborhoodTransform)]
) extends PolytreeParseFeature {

  @transient val eventStatistics: Seq[(String, String, NeighborhoodExtractor, NeighborhoodEventStatistic)] = {

    for {
      (neighborhoodName, extractor, counts) <- neighborhoodCounts
      (transformName, transform) <- transforms
    } yield {
      val stat = NeighborhoodEventStatistic("", counts, transform)
      (neighborhoodName, transformName, extractor, stat)
    }
  }

  override def apply(parse: PolytreeParse, score: Double): MLFeatureVector = {
    MLFeatureVector(
      for {
        (neighborhoodName, transformName, extractor, stat) <- eventStatistics
      } yield {
        val featureValue = extractor(parse) map { neighborhood =>
          -math.log(stat.getSmoothedEventProbability(neighborhood))
        } reduce { (x, y) => x + y }
        MLFeatureName(
          List('eventStat, Symbol(neighborhoodName), Symbol(transformName))
        ) -> featureValue
      }
    )
  }
}

/** Simply passes along the length of the sentence as a feature. */
case object SentenceLengthFeature extends PolytreeParseFeature {

  override def apply(parse: PolytreeParse, score: Double): MLFeatureVector = {
    MLFeatureVector(Seq(MLFeatureName(List(name)) -> parse.sentence.tokens.tail.size))
  }

  val name: Symbol = 'sentLen
}

/** Simply passes along the original score of the parse as a feature. */
case object BaseParserScoreFeature extends PolytreeParseFeature {

  override def apply(parse: PolytreeParse, score: Double): MLFeatureVector = {
    MLFeatureVector(Seq(MLFeatureName(List(name)) -> score))
  }

  val name: Symbol = 'baseParserScore
}

/** A PolytreeParseFeatureUnion merges the output of a list of features.
  *
  * @param features a list of the features we want to merge into a single feature
  */
case class PolytreeParseFeatureUnion(
    val features: Seq[PolytreeParseFeature]
) extends PolytreeParseFeature {

  override def apply(parse: PolytreeParse, score: Double): MLFeatureVector = {
    features map (f => f(parse, score)) reduce ((m1, m2) => MLFeatureVector.mergeVectors(m1, m2))
  }
}
