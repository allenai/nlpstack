package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters

abstract class DependencyParsingTransitionSystem(brownClusters: Seq[BrownClusters] = Seq())
    extends TransitionSystem {

  @transient
  override val taskIdentifier: TaskIdentifier = ArcHybridTaskIdentifier

  @transient private val tokenFeatureTagger = new TokenFeatureTagger(Seq(
    TokenPositionFeature,
    TokenPropertyFeature('factorieCpos),
    TokenPropertyFeature('factoriePos),
    TokenPropertyFeature('brown0),
    KeywordFeature(DependencyParsingTransitionSystem.keywords)
  ))

  override def initialState(
    marbleBlock: MarbleBlock,
    constraints: Seq[TransitionConstraint]
  ): Option[State] = {

    val sentence: Option[Sentence] = marbleBlock match {
      case parse: PolytreeParse =>
        Some(parse.sentence)
      case sentence: Sentence =>
        Some(sentence)
      case _ => None
    }
    val augmentedSentence: Option[AnnotatedSentence] = sentence map { sent =>
      val taggedSentence = sent.taggedWithFactorie
        .taggedWithBrownClusters(brownClusters)
      tokenFeatureTagger.tag(taggedSentence)

      // override factorie tags with requested tags
      val requestedCposConstraints: Map[Int, RequestedCpos] =
        (constraints flatMap { constraint: TransitionConstraint =>
          constraint match {
            case cposConstraint: RequestedCpos => Some(cposConstraint)
            case _ => None
          }
        } map { constraint =>
          (constraint.tokenIndex, constraint)
        }).toMap
      val overriddenSentence: Sentence = Sentence(
        Range(0, taggedSentence.tokens.size) map { i =>
          requestedCposConstraints.get(i)
        } zip taggedSentence.tokens map {
          case (maybeConstraint, tok) =>
            maybeConstraint match {
              case Some(constraint) =>
                tok.updateProperties(Map(
                  'factorieCpos -> Set(constraint.cpos),
                  'factoriePos -> Set()
                ))
              case None => tok
            }
        }
      )
      tokenFeatureTagger.tag(overriddenSentence)
    }

    augmentedSentence map { augSent => systemSpecificInitialState(augSent) }
  }

  protected def systemSpecificInitialState(augmentedSentence: AnnotatedSentence): State

  override def toSculpture(state: State): Option[Sculpture] = {
    state match {
      case tpState: TransitionParserState =>
        tpState.asSculpture
      case _ =>
        None
    }
  }
}

object DependencyParsingTransitionSystem {

  val keywords = WordClusters.commonWords ++ WordClusters.puncWords ++
    WordClusters.stopWords
}
