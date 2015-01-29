package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }
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
      case ttFeature: TokenTransformFeature =>
        JsObject(tokenTransformFeatureFormat.write(ttFeature).asJsObject.fields +
          ("type" -> JsString("TokenTransformFeature")))
      case tsFeature: OfflineTokenFeature =>
        JsObject(offlineTokenFeatureFormat.write(tsFeature).asJsObject.fields +
          ("type" -> JsString("OfflineTokenFeature")))
      case obFeature: OfflineBinaryTokenFeature =>
        JsObject(offlineBinaryTokenFeatureFormat.write(obFeature).asJsObject.fields +
          ("type" -> JsString("OfflineBinaryTokenFeature")))
      case otFeature: OfflineTernaryTokenFeature =>
        JsObject(offlineTernaryTokenFeatureFormat.write(otFeature).asJsObject.fields +
          ("type" -> JsString("OfflineTernaryTokenFeature")))
      case tcFeature: TokenCardinalityFeature =>
        JsObject(tokenCardinalityFeatureFormat.write(tcFeature).asJsObject.fields +
          ("type" -> JsString("TokenCardinalityFeature")))
      case featureUnion: FeatureUnion =>
        JsObject(featureUnionFormat.write(featureUnion).asJsObject.fields +
          ("type" -> JsString("FeatureUnion")))
      case conFeature: ConstantFeature =>
        JsObject(constantFeatureFormat.write(conFeature).asJsObject.fields +
          ("type" -> JsString("ConstantFeature")))
    }

    def read(value: JsValue): StateFeature = value match {
      case JsObject(values) => values("type") match {
        case JsString("TokenTransformFeature") => tokenTransformFeatureFormat.read(value)
        case JsString("OfflineTokenFeature") => offlineTokenFeatureFormat.read(value)
        case JsString("OfflineBinaryTokenFeature") => offlineBinaryTokenFeatureFormat.read(value)
        case JsString("OfflineTernaryTokenFeature") => offlineTernaryTokenFeatureFormat.read(value)
        case JsString("TokenCardinalityFeature") => tokenCardinalityFeatureFormat.read(value)
        case JsString("FeatureUnion") => featureUnionFormat.read(value)
        case JsString("ConstantFeature") => constantFeatureFormat.read(value)
        case x => deserializationError(s"Invalid identifier for TransitionParserFeature: $x")
      }
      case _ => deserializationError("Unexpected JsValue type. Must be JsObject.")
    }
  }

  val tokenTransformFeatureFormat: RootJsonFormat[TokenTransformFeature] =
    jsonFormat2(TokenTransformFeature.apply)
  val offlineTokenFeatureFormat: RootJsonFormat[OfflineTokenFeature] =
    jsonFormat1(OfflineTokenFeature.apply)
  val offlineBinaryTokenFeatureFormat: RootJsonFormat[OfflineBinaryTokenFeature] =
    jsonFormat2(OfflineBinaryTokenFeature.apply)
  val offlineTernaryTokenFeatureFormat: RootJsonFormat[OfflineTernaryTokenFeature] =
    jsonFormat3(OfflineTernaryTokenFeature.apply)
  val tokenCardinalityFeatureFormat: RootJsonFormat[TokenCardinalityFeature] =
    jsonFormat1(TokenCardinalityFeature.apply)
  val featureUnionFormat: RootJsonFormat[FeatureUnion] = jsonFormat1(FeatureUnion.apply)
  val constantFeatureFormat: RootJsonFormat[ConstantFeature] = jsonFormat1(ConstantFeature.apply)
}

case class ConstantFeature(symbol: Symbol) extends StateFeature {
  override def apply(state: State): FeatureVector = {
    FeatureVector(Seq((FeatureName(List(symbol)), 1.0)))
  }
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
