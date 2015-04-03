package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.ml.FeatureVector
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A TransitionClassifier maps Transitions to probabilities. */
abstract class TransitionClassifier {

  /** Returns the most probable Transition according to .getDistribution(featureVector).
    *
    * @param featureVector the feature vector to use to compute the distribution
    * @return the most probable Transition, given the argument feature vector
    */
  def classify(featureVector: FeatureVector): StateTransition

  /** Given the argument feature vector, this assigns a probability to a set of Transitions.
    *
    * @param featureVector the feature vector to use to compute the distribution
    * @return a probability distribution over Transitions
    */
  def getDistribution(featureVector: FeatureVector): Map[StateTransition, Float]

}

/** Companion class for serializing TransitionClassifier instances. */
object TransitionClassifier {

  /** Boilerplate code to serialize a TransitionClassifier to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM TransitionClassifier, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object TransitionClassifierJsonFormat extends RootJsonFormat[TransitionClassifier] {
    implicit val embeddedClassifierFormat =
      jsonFormat4(EmbeddedClassifier.apply).pack("type" -> "EmbeddedClassifier")
    //implicit val adaptiveDecisionTreeClassifierFormat =
    //  jsonFormat4(AdaptiveDecisionTreeClassifier.apply).pack(
    //    "type" -> "AdaptiveDecisionTreeClassifier")

    def write(classifier: TransitionClassifier): JsValue = classifier match {
      case embClassifier: EmbeddedClassifier => embClassifier.toJson
      //case adtClassifier: AdaptiveDecisionTreeClassifier => adtClassifier.toJson
      case x => deserializationError(s"Cannot serialize this classifier type: $x")
    }

    def read(value: JsValue): TransitionClassifier = value.asJsObject.unpackWith(
      embeddedClassifierFormat
    )
    //adaptiveDecisionTreeClassifierFormat)
  }
}

