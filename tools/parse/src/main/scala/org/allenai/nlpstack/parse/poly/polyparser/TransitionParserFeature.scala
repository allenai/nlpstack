package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.fsm.{ State, StateFeature }
import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }

/** A TokenTransformFeature creates a TransitionParserFeature from a TokenTransform and a StateRef.
  *
  * Essentially it simply applies the TokenTransform to the token referenced by the StateRef (see
  * definitions of TokenTransform and StateRef for details).
  *
  * For instance, suppose we want a binary feature that gives us the word at the top of the stack.
  * We can achieve this with TokenTransformFeature(StackRef(0), WordTransform).
  *
  * @param stateRef the StateRef that refers to our desired token
  * @param tokenTransforms the transformation we want to perform on our desired token
  */
case class TokenTransformFeature(val stateRef: StateRef, val tokenTransforms: Set[TokenTransform])
    extends StateFeature {

  override def apply(state: State): FeatureVector = {
    state match {
      case tpState: TransitionParserState =>
        FeatureVector(for {
          tokenIndex <- stateRef(tpState).toSeq
          tokenTrans <- tokenTransforms
          sym: Symbol <- tokenTrans(tpState, tokenIndex)
        } yield (FeatureName(List(tokenTrans.name, stateRef.name, sym)), 1.0))
    }
  }
  override def toString: String = s"tokenTransformFeature.${stateRef.name}"
}

case class OfflineTokenFeature(val stateRef: StateRef)
    extends StateFeature {

  override def apply(state: State): FeatureVector = {
    state match {
      case tpState: TransitionParserState =>
        FeatureVector(
          for {
            tokenIndex <- stateRef(tpState).toSeq
            origFeatureVectorMapping <- tpState.annotatedSentence.annotation(tokenIndex).values
          } yield FeatureName(stateRef.name +: (origFeatureVectorMapping._1).symbols) -> 1.0
        )
    }
  }

  override def toString: String = s"offlineTokenFeature.${stateRef.name}"
}

case class OfflineBinaryTokenFeature(val stateRef1: StateRef, val stateRef2: StateRef)
    extends StateFeature {

  override def apply(state: State): FeatureVector = {
    state match {
      case tpState: TransitionParserState =>
        FeatureVector(
          for {
            tokenIndex1 <- stateRef1(tpState).toSeq
            origFeatureVectorMapping1 <- tpState.annotatedSentence.annotation(tokenIndex1).values
            tokenIndex2 <- stateRef2(tpState).toSeq
            origFeatureVectorMapping2 <- tpState.annotatedSentence.annotation(tokenIndex2).values
          } yield {
            val newFeatureName = (stateRef1.name +: (origFeatureVectorMapping1._1).symbols) ++
              (stateRef2.name +: (origFeatureVectorMapping2._1).symbols)
            FeatureName(newFeatureName) -> 1.0
          }
        )
    }
  }

  override def toString: String = s"offlineTokenFeature.${stateRef1.name}.${stateRef2.name}"
}

case class TokenCardinalityFeature(val stateRefs: Seq[StateRef])
    extends StateFeature {

  @transient val featureName = 'tokenCardinality

  override def apply(state: State): FeatureVector = {
    state match {
      case tpState: TransitionParserState =>
        val featureNames: Seq[FeatureName] = stateRefs map { stateRef =>
          FeatureName(List(featureName, stateRef.name, Symbol(stateRef(tpState).size.toString)))
        }
        FeatureVector(
          featureNames map { featureName =>
            (featureName, 1.0)
          }
        )
    }
  }

  override def toString: String = s"tokenCardinalityFeature"
}

