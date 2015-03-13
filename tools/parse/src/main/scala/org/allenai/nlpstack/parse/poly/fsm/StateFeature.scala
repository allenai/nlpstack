package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.FeatureVector
import org.allenai.nlpstack.parse.poly.polyparser._
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A StateFeature computes a feature vector corresponding to a given parser state. */
abstract class StateFeature extends (State => FeatureVector)

object StateFeature {

  /** Boilerplate code to serialize a TransitionParserFeature to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM TransitionParserFeature, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object StateFeatureJsonFormat
      extends RootJsonFormat[StateFeature] {

    def write(feature: StateFeature): JsValue = feature match {
      case PreviousLinkDirection => JsString("PreviousLinkDirection")
      case ttFeature: TokenTransformFeature =>
        JsObject(tokenTransformFeatureFormat.write(ttFeature).asJsObject.fields +
          ("type" -> JsString("TokenTransformFeature")))
      case tsFeature: OfflineTokenFeature =>
        JsObject(offlineTokenFeatureFormat.write(tsFeature).asJsObject.fields +
          ("type" -> JsString("OfflineTokenFeature")))
      case tcFeature: TokenCardinalityFeature =>
        JsObject(tokenCardinalityFeatureFormat.write(tcFeature).asJsObject.fields +
          ("type" -> JsString("TokenCardinalityFeature")))
      case featureUnion: FeatureUnion =>
        JsObject(featureUnionFormat.write(featureUnion).asJsObject.fields +
          ("type" -> JsString("FeatureUnion")))
    }

    def read(value: JsValue): StateFeature = value match {
      case JsString(typeid) => typeid match {
        case "PreviousLinkDirection" => PreviousLinkDirection
        case x => deserializationError(s"Invalid identifier for Transition: $x")
      }
      case JsObject(values) => values("type") match {
        case JsString("TokenTransformFeature") => tokenTransformFeatureFormat.read(value)
        case JsString("OfflineTokenFeature") => offlineTokenFeatureFormat.read(value)
        case JsString("TokenCardinalityFeature") => tokenCardinalityFeatureFormat.read(value)
        case JsString("FeatureUnion") => featureUnionFormat.read(value)
        case x => deserializationError(s"Invalid identifier for TransitionParserFeature: $x")
      }
      case _ => deserializationError("Unexpected JsValue type. Must be JsObject.")
    }
  }

  val tokenTransformFeatureFormat: RootJsonFormat[TokenTransformFeature] =
    jsonFormat2(TokenTransformFeature.apply)
  val offlineTokenFeatureFormat: RootJsonFormat[OfflineTokenFeature] =
    jsonFormat2(OfflineTokenFeature.apply)
  val tokenCardinalityFeatureFormat: RootJsonFormat[TokenCardinalityFeature] =
    jsonFormat1(TokenCardinalityFeature.apply)
  val featureUnionFormat: RootJsonFormat[FeatureUnion] = jsonFormat1(FeatureUnion.apply)
}

/** A FeatureUnion simply merges the output of a list of features.
  *
  * @param features a list of the features we want to merge into a single feature
  */
case class FeatureUnion(val features: Iterable[StateFeature])
    extends StateFeature {

  override def apply(state: State): FeatureVector = {
    features map { f =>
      f(state)
    } reduce { (m1, m2) =>
      FeatureVector(m1.values ++ m2.values)
    }
  }
}
