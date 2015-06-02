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

/** The TokenLinkFeature makes a feature for every arc label between any pair of tokens from two
  * token sets.
  *
  * @param stateRef1 the first token set
  * @param stateRef2 the second token set
  */
case class TokenLinkFeature(stateRef1: StateRef, stateRef2: StateRef)
    extends StateFeature {

  override def apply(state: State): FeatureVector = {
    state match {
      case tpState: TransitionParserState =>
        FeatureVector(
          for {
            tokenIndex1 <- stateRef1(tpState).toSeq
            tokenIndex2 <- stateRef2(tpState).toSeq
            arcLabel <- tpState.arcLabels.get(Set(tokenIndex1, tokenIndex2))
          } yield {
            FeatureName(Seq(stateRef1.name, Symbol("to"),
              stateRef2.name, Symbol("label"), arcLabel.toSymbol)) -> 1.0
          }
        )
    }
  }

  override def toString(): String = s"tokenLinkFeature.${stateRef1.name}.${stateRef2.name}"
}

case object PreviousLinkDirection extends StateFeature {
  override def apply(state: State): FeatureVector = {
    state match {
      case tpState: TransitionParserState =>
        tpState.previousLink match {
          case Some((crumb, gretel)) => FeatureVector(
            Seq(FeatureName(List('prevLink, Symbol((crumb < gretel).toString))) -> 1.0)
          )
          case None => FeatureVector(Seq())
        }
    }
  }

  override def toString(): String = s"prevLink"
}

case class TokenCardinalityFeature(val stateRefs: Seq[StateRef])
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
