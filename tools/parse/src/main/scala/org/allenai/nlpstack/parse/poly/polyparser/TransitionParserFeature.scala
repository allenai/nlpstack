package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.AnnotatedSentence
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
case class TokenTransformFeature(stateRef: StateRef, tokenTransforms: Set[TokenTransform])
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
  override def toString(): String = s"tokenTransformFeature.${stateRef.name}"
}

/** An OfflineTokenFeature creates a StateFeature from an AnnotatedSentence and a StateRef.
  *
  * Essentially it simply takes the pre-annotated features from the token referenced by
  * the StateRef (see definitions of AnnotatedSentence and StateRef for details).
  *
  * For instance, suppose we want the pre-annotated feature corresponding to
  * the word at the top of the stack. We can achieve this with
  * OfflineTokenFeature(annotatedSentence, StackRef(0)).
  *
  * @param annotatedSentence a sentence whose tokens are annotated with features
  * @param stateRef the StateRef that refers to our desired token(s)
  */
case class OfflineTokenFeature(annotatedSentence: AnnotatedSentence, stateRef: StateRef)
    extends StateFeature {

  override def apply(state: State): FeatureVector = {
    FeatureVector(
      for {
        tokenIndex <- stateRef(state).toSeq
        origFeatureVectorMapping <- annotatedSentence.annotation(tokenIndex).values
      } yield FeatureName(stateRef.name +: origFeatureVectorMapping._1.symbols) -> 1.0
    )
  }

  override def toString(): String = s"offlineTokenFeature.${stateRef.name}"
}

/** An TokenCardinalityFeature creates a FeatureVector with features for the cardinality of the
  * the set of tokens referenced by a StateRef.
  *
  * For instance, suppose we want a feature that tells us the number of children of the node
  * on top of the stack. We can achieve this with
  * TokenCardinalityFeature(Seq(TransitiveRef(StackRef(0), Seq(TokenGretels))))
  *
  * @param stateRefs the StateRefs that we want to know the cardinalities of
  */
case class TokenCardinalityFeature(stateRefs: Seq[StateRef])
    extends StateFeature {

  @transient val featureName = 'card

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

  override def toString(): String = s"tokenCardinalityFeature"
}
