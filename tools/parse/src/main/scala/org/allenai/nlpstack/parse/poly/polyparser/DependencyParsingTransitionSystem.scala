package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ SentenceTransform, AnnotatedSentence, WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters

/** A struct contains the "modes" of a typical transition parser.
  *
  * Specifically, the mode tells you what the next expected action is.
  */
object DependencyParserModes {
  val TRANSITION: Int = 0
  val LEFTLABEL: Int = 1
  val RIGHTLABEL: Int = 2
}

abstract class DependencyParsingTransitionSystem(
    marbleBlock: MarbleBlock,
    constraints: Set[TransitionConstraint],
    taggers: Seq[SentenceTransform]
) extends TransitionSystem {

  @transient
  override val taskIdentifier: TaskIdentifier = ArcHybridTaskIdentifier

  @transient val sentence: Sentence =
    marbleBlock match {
      case parse: PolytreeParse =>
        parse.sentence
      case sentence: Sentence =>
        sentence
    }

  val annotatedSentence: AnnotatedSentence = {
    val taggedSentence = taggers.foldLeft(sentence)((sent, tagger) => tagger.transform(sent))

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
    val tokenFeatureTagger = new TokenFeatureTagger(Seq(
      TokenPositionFeature,
      TokenPropertyFeature('factorieCpos),
      TokenPropertyFeature('factoriePos),
      TokenPropertyFeature('brown0),
      KeywordFeature(DependencyParsingTransitionSystem.keywords)
    ))
    tokenFeatureTagger.tag(overriddenSentence)
  }

  val labelingFeature =
    FeatureUnion(List(
      new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), StackRef(2), BufferRef(0),
        BufferRef(1), PreviousLinkCrumbRef, PreviousLinkGretelRef, PreviousLinkCrumbGretelRef,
        PreviousLinkGrandgretelRef,
        TransitiveRef(StackRef(0), Seq(TokenGretels)),
        TransitiveRef(StackRef(1), Seq(TokenGretels)),
        TransitiveRef(BufferRef(0), Seq(TokenGretels)),
        TransitiveRef(StackRef(0), Seq(TokenCrumb)),
        StackLeftGretelsRef(0), StackRightGretelsRef(0))),
      new OfflineTokenFeature(annotatedSentence, StackRef(0)),
      new OfflineTokenFeature(annotatedSentence, StackRef(1)),
      new OfflineTokenFeature(annotatedSentence, StackRef(2)),
      new OfflineTokenFeature(annotatedSentence, BufferRef(0)),
      new OfflineTokenFeature(annotatedSentence, BufferRef(1)),
      new OfflineTokenFeature(annotatedSentence, PreviousLinkCrumbRef),
      new OfflineTokenFeature(annotatedSentence, PreviousLinkGretelRef),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(PreviousLinkCrumbRef, Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(PreviousLinkGretelRef, Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(0), Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(1), Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, TransitiveRef(BufferRef(0), Seq(TokenGretels))),
      new OfflineTokenFeature(annotatedSentence, StackLeftGretelsRef(0)),
      new OfflineTokenFeature(annotatedSentence, StackRightGretelsRef(0)),
      new OfflineTokenFeature(annotatedSentence, FirstRef),
      new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords)))
    ))

  val defaultFeature = FeatureUnion(List(
    new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), BufferRef(0), BufferRef(1),
      TransitiveRef(StackRef(0), Seq(TokenGretels)),
      TransitiveRef(StackRef(1), Seq(TokenGretels)),
      TransitiveRef(BufferRef(0), Seq(TokenGretels)),
      TransitiveRef(StackRef(0), Seq(TokenCrumb)),
      StackLeftGretelsRef(0), StackRightGretelsRef(1))),
    new OfflineTokenFeature(annotatedSentence, StackRef(0)),
    new OfflineTokenFeature(annotatedSentence, StackRef(1)),
    new OfflineTokenFeature(annotatedSentence, BufferRef(0)),
    new OfflineTokenFeature(annotatedSentence, BufferRef(1)),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(0), Seq(TokenGretels))),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(1), Seq(TokenGretels))),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(BufferRef(0), Seq(TokenGretels))),
    new OfflineTokenFeature(annotatedSentence, TransitiveRef(StackRef(0), Seq(TokenCrumb))),
    new OfflineTokenFeature(annotatedSentence, FirstRef),
    new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords)))
  ))

  override def initialState(
    constraints: Seq[TransitionConstraint]
  ): Option[State] = {
    Some(systemSpecificInitialState(sentence))
  }

  protected def systemSpecificInitialState(sentence: Sentence): State

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

/** The LabelLeftArc operator labels the most recently created left-facing arc. */
case class LabelLeftArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == DependencyParserModes.LEFTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    val (crumb, gretel) = state.previousLink.get
    if (PolytreeParse.arcInverterStanford.inverseArcLabels.contains(label)) {
      val gretelChildren: Set[Int] =
        state.children.getOrElse(gretel, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (gretel -> (gretelChildren + crumb))
      )
    } else {
      val crumbChildren: Set[Int] =
        state.children.getOrElse(crumb, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (crumb -> (crumbChildren + gretel))
      )
    }
  }

  @transient
  override val name: String = s"LtLbl[${label.name}]"
}

/** The LabelRightArc operator labels the most recently created right-facing arc. */
case class LabelRightArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == DependencyParserModes.RIGHTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    val (crumb, gretel) = state.previousLink.get
    if (PolytreeParse.arcInverterStanford.inverseArcLabels.contains(label)) {
      val gretelChildren: Set[Int] =
        state.children.getOrElse(gretel, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (gretel -> (gretelChildren + crumb))
      )
    } else {
      val crumbChildren: Set[Int] =
        state.children.getOrElse(crumb, Set.empty[Int])
      state.copy(
        arcLabels = state.arcLabels + (Set(crumb, gretel) -> label),
        parserMode = DependencyParserModes.TRANSITION,
        children = state.children + (crumb -> (crumbChildren + gretel))
      )
    }
  }

  @transient
  override val name: String = s"RtLbl[${label.name}]"
}
