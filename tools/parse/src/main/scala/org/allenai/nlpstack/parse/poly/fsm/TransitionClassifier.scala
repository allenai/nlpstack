package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.FeatureVector

import reming.DefaultJsonProtocol._

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
  private implicit val embeddedClassifierFormat = jsonFormat4(EmbeddedClassifier.apply)

  implicit val transitionClassifierJsonFormat = parentFormat[TransitionClassifier](
    childFormat[EmbeddedClassifier, TransitionClassifier]
  )
}

