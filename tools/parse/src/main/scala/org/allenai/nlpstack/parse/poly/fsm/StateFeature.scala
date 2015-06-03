package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.FeatureVector
import org.allenai.nlpstack.parse.poly.polyparser._

import reming.LazyFormat
import reming.DefaultJsonProtocol._

/** A StateFeature computes a feature vector corresponding to a given parser state. */
abstract class StateFeature extends (State => FeatureVector)

object StateFeature {
  private implicit val tokenTransformFeatureFormat = jsonFormat2(TokenTransformFeature.apply)
  private implicit val offlineTokenFeatureFormat = jsonFormat2(OfflineTokenFeature.apply)
  private implicit val tokenCardinalityFeatureFormat = jsonFormat1(TokenCardinalityFeature.apply)

  implicit object StateFeatureJsonFormat extends LazyFormat[StateFeature] {
    private implicit val featureUnionFormat = jsonFormat1(FeatureUnion.apply)

    override val delegate = parentFormat[StateFeature](
      childFormat[TokenTransformFeature, StateFeature],
      childFormat[OfflineTokenFeature, StateFeature],
      childFormat[TokenCardinalityFeature, StateFeature],
      childFormat[FeatureUnion, StateFeature]
    )
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
