package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ WordClusters, AnnotatedSentence, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, BrownClusters }

/** The ArcHybridTaskIdentifier identifies the ClassificationTask associated with a particular
  * state of the arc-hybrid transition system.
  */
object ArcHybridTaskIdentifier extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    state match {
      case tpState: TransitionParserState =>
        tpState.parserMode match {
          case DependencyParserModes.TRANSITION =>
            Some(ApplicabilitySignature(
              StateTransition.applicable(ArcHybridShift, Some(state)),
              false,
              StateTransition.applicable(ArcHybridLeftArc('NONE), Some(state)),
              StateTransition.applicable(ArcHybridRightArc('NONE), Some(state))
            ))
          case DependencyParserModes.LEFTLABEL =>
            Some(SimpleTask("dt-leftlabel"))
          case DependencyParserModes.RIGHTLABEL =>
            Some(SimpleTask("dt-rightlabel"))
          case _ => None
        }
      case _ => None
    }
  }
}

/** An ArcHybridTransitionSystem has three transition operators: Shift, RightArc, and LeftArc.
  *
  * The Shift operator behaves the same as in the ArcEagerTransitionSystem:
  * it pops the next buffer item and pushes it onto the stack.
  *
  * The RightArc operator creates an arc from the second element of the stack to the top element
  * of the stack, then pops the top of the stack.
  *
  * The LeftArc operator creates an arc from the next buffer item to the top element
  * of the stack, then pops the top of the stack.
  *
  * An important property of the ArcHybridTransitionSystem is that the only element that can
  * get a breadcrumb via an operator is the top of the stack. Thus the stack top is the "focal
  * point" of this transition system.
  *
  * @param brownClusters an optional set of Brown clusters to use for creating features
  */
case class ArcHybridTransitionSystem(
    marbleBlock: MarbleBlock,
    brownClusters: Seq[BrownClusters] = Seq()
) extends DependencyParsingTransitionSystem(marbleBlock, brownClusters) {

  @transient
  override val taskIdentifier: TaskIdentifier = ArcHybridTaskIdentifier

  @transient private val tokenFeatureTagger = new TokenFeatureTagger(Seq(
    TokenPositionFeature,
    TokenPropertyFeature('factorieCpos),
    TokenPropertyFeature('factoriePos),
    TokenPropertyFeature('brown0),
    KeywordFeature(DependencyParsingTransitionSystem.keywords)
  ))

  val annotatedSentence: AnnotatedSentence = {
    val taggedSentence = sentence.taggedWithFactorie
      .taggedWithBrownClusters(brownClusters)
      .taggedWithLexicalProperties
    tokenFeatureTagger.tag(taggedSentence)

    // TODO: re-enable
    // override factorie tags with requested tags
    /*
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
    */
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
      new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords))),
      new TokenTransformFeature(FirstRef, Set(TokenPropertyTransform('factorieCpos)))
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
    new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords))),
    new TokenTransformFeature(FirstRef, Set(TokenPropertyTransform('factorieCpos)))
  ))

  override def computeFeature(state: State): FeatureVector = {
    (taskIdentifier(state) map { ident => ident.filenameFriendlyName }) match {
      case Some(x) if x.startsWith("dt-") => labelingFeature(state)
      case _ => defaultFeature(state)
    }
  }

  override def systemSpecificInitialState(
    sentence: Sentence
  ): TransitionParserState = {
    new TransitionParserState(Vector(), 1, Map(0 -> -1), Map(), Map(), sentence,
      None, DependencyParserModes.TRANSITION)
  }

  override def guidedCostFunction(goldObj: Sculpture): Option[StateCostFunction] =
    goldObj match {
      case parse: PolytreeParse =>
        Some(new ArcHybridGuidedCostFunction(parse, this))
      case _ => None
    }

  override def interpretConstraint(
    constraint: TransitionConstraint
  ): ((State, StateTransition) => Boolean) = {
    constraint match {
      case forbiddenArc: ForbiddenEdge => ArcHybridForbiddenArcInterpretation(forbiddenArc)
      case requestedArc: RequestedArc => ArcHybridRequestedArcInterpretation(requestedArc)
      case forbiddenArcLabel: ForbiddenArcLabel =>
        ArcHybridForbiddenArcLabelInterpretation(forbiddenArcLabel)
      case _ => TransitionSystem.trivialConstraint
    }
  }
}

/** The ArcHybridShift operator pops the next buffer item and pushes it onto the stack. */
case object ArcHybridShift extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == DependencyParserModes.TRANSITION &&
      (state.bufferPosition < state.sentence.size) &&
      !(state.bufferPosition == 0 && state.stack.nonEmpty)
  }

  override def advanceState(state: TransitionParserState): State = {
    val nextBufferPosition =
      if (state.bufferPosition == 0) {
        state.sentence.size
      } else if (state.bufferPosition == state.sentence.size - 1) {
        0
      } else {
        state.bufferPosition + 1
      }
    state.copy(
      stack = state.bufferPosition +: state.stack,
      bufferPosition = nextBufferPosition
    )
  }

  override val name: String = "Sh"
}

/** The ArcHybridLeftArc operator creates an arc from the next buffer item to the stack top
  * and then performs a Reduce.
  *
  * @param label the label to attach to the created arc
  */
case class ArcHybridLeftArc(val label: Symbol = 'NONE) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    (state.parserMode == DependencyParserModes.TRANSITION) && ArcHybridLeftArc.applicable(state)
  }

  override def advanceState(state: TransitionParserState): State = {
    state.copy(
      stack = state.stack.tail,
      breadcrumb = state.breadcrumb + (state.stack.head -> state.bufferPosition),
      arcLabels = state.arcLabels +
      (Set(state.stack.head, state.bufferPosition) -> label),
      previousLink = Some((state.bufferPosition, state.stack.head)),
      parserMode = DependencyParserModes.LEFTLABEL
    )
  }

  @transient
  override val name: String = s"Lt[${label.name}]"
}

object ArcHybridLeftArc {
  def applicable(state: State): Boolean = {
    state match {
      case tpState: TransitionParserState =>
        (tpState.bufferPosition < tpState.sentence.size) && tpState.stack.nonEmpty
      case _ => false
    }
  }
}

/** The ArcHybridRightArc operator creates an arc from the stack's second element to the stack top
  * and then performs a Reduce.
  *
  * @param label the label to attach to the created arc
  */
case class ArcHybridRightArc(val label: Symbol = 'NONE) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    (state.parserMode == DependencyParserModes.TRANSITION) && ArcHybridRightArc.applicable(state)
  }

  override def advanceState(state: TransitionParserState): State = {
    val stackFirst = state.stack.head
    val stackSecond = state.stack.tail.head
    val result = state.copy(
      stack = state.stack.tail,
      breadcrumb = state.breadcrumb + (stackFirst -> stackSecond),
      arcLabels = state.arcLabels +
      (Set(stackFirst, stackSecond) -> label),
      previousLink = Some((stackSecond, stackFirst)),
      parserMode = DependencyParserModes.RIGHTLABEL
    )
    result
  }

  @transient
  override val name: String = s"Rt[${label.name}]"
}

object ArcHybridRightArc {
  def applicable(state: State): Boolean = {
    state match {
      case tpState: TransitionParserState =>
        tpState.stack.size >= 2
      case _ => false
    }
  }
}

/** The ArcHybridGuidedCostFunction uses a gold parse tree to make deterministic decisions
  * about which transition to apply in any given state. Since the decision is uniquely determined
  * by the gold parse, the returned map will have only a single mapping that assigns zero cost
  * to the correct transition (all other transitions therefore have an implicit cost of infinity).
  *
  * @param parse the gold parse tree
  */
class ArcHybridGuidedCostFunction(
    parse: PolytreeParse,
    override val transitionSystem: TransitionSystem
) extends StateCostFunction {

  override def apply(state: State): Map[StateTransition, Double] = {
    val result: Map[StateTransition, Double] = state match {
      case tpState: TransitionParserState =>
        require(!tpState.isFinal)
        if (tpState.parserMode == DependencyParserModes.LEFTLABEL) {
          val (crumb, gretel) = tpState.previousLink.get
          val arclabel = parse.arcLabelByEndNodes(Set(crumb, gretel))
          Map(LabelLeftArc(arclabel) -> 0)
        } else if (tpState.parserMode == DependencyParserModes.RIGHTLABEL) {
          val (crumb, gretel) = tpState.previousLink.get
          val arclabel = parse.arcLabelByEndNodes(Set(crumb, gretel))
          Map(LabelRightArc(arclabel) -> 0)
        } else if (tpState.stack.isEmpty) {
          Map(ArcHybridShift -> 0)
        } else if (tpState.bufferIsEmpty) {
          Map()
        } else {
          val stackFirst = tpState.stack.head
          val bufferFirst = tpState.bufferPosition
          if (parse.breadcrumb(stackFirst) == bufferFirst) {
            Map(ArcHybridLeftArc() -> 0)
          } else if (tpState.stack.size < 2) {
            Map(ArcHybridShift -> 0)
          } else {
            val stackSecond = tpState.stack.tail.head
            if (parse.breadcrumb(stackFirst) == stackSecond &&
              parse.getGretels(stackFirst) == tpState.getGretels(stackFirst)) {
              Map(ArcHybridRightArc() -> 0)
            } else {
              Map(ArcHybridShift -> 0)
            }
          }
        }
    }
    result
  }
}

/** The ArcHybridForbiddenArcInterpretation handles ForbiddenArc constraints for the arc hybrid
  * system. In other words, it translates these constraints into a function that returns true
  * for any (state, transition) pair that violates the constraint.
  *
  * @param forbiddenArc the forbidden arc constraint to consider
  */
case class ArcHybridForbiddenArcInterpretation(
    forbiddenArc: ForbiddenEdge
) extends ParsingConstraintInterpretation {

  def applyToParserState(state: TransitionParserState, transition: StateTransition): Boolean = {
    transition match {
      case ArcHybridLeftArc(_) =>
        (StackRef(0)(state).headOption, BufferRef(0)(state).headOption) match {
          case (Some(stackFirst), Some(bufferFirst)) =>
            Set(forbiddenArc.token1, forbiddenArc.token2) == Set(stackFirst, bufferFirst)
          case _ => false
        }
      case ArcHybridRightArc(_) =>
        (StackRef(0)(state).headOption, StackRef(1)(state).headOption) match {
          case (Some(stackFirst), Some(stackSecond)) =>
            Set(forbiddenArc.token1, forbiddenArc.token2) == Set(stackFirst, stackSecond)
          case _ => false
        }
      case _ => false
    }
  }
}

/** The ArcHybridRequestedArcInterpretation handles RequestedArc constraints for the arc hybrid
  * system. In other words, it translates these constraints into a function that returns true
  * for any (state, transition) pair that violates the constraint.
  *
  * @param requestedArc the requested arc cosntraint to consider
  */
case class ArcHybridRequestedArcInterpretation(
    requestedArc: RequestedArc
) extends ParsingConstraintInterpretation {

  private val arcTokens: Set[Int] = Set(requestedArc.token1, requestedArc.token2)

  def applyToParserState(state: TransitionParserState, transition: StateTransition): Boolean = {
    transition match {
      case ArcHybridLeftArc(_) =>
        (StackRef(0)(state).headOption, BufferRef(0)(state).headOption) match {
          case (Some(stackFirst), Some(bufferFirst)) =>
            val otherToken = (arcTokens - stackFirst).head
            arcTokens.contains(stackFirst) &&
              !arcTokens.contains(bufferFirst) &&
              (state.stack.contains(otherToken) ||
                otherToken == 0 ||
                (state.bufferPosition > 0 && state.bufferPosition <= otherToken))
          case _ => false
        }
      case ArcHybridRightArc(_) =>
        (StackRef(0)(state).headOption, StackRef(1)(state).headOption) match {
          case (Some(stackFirst), Some(stackSecond)) =>
            val otherToken = (arcTokens - stackFirst).head
            arcTokens.contains(stackFirst) &&
              !arcTokens.contains(stackSecond) &&
              (state.stack.contains(otherToken) ||
                otherToken == 0 ||
                (state.bufferPosition > 0 && state.bufferPosition <= otherToken))
          case _ => false
        }
      case LabelLeftArc(arcLabel) =>
        (
          PreviousLinkCrumbRef(state).headOption,
          PreviousLinkGretelRef(state).headOption, requestedArc.arcLabel
        ) match {
            case (Some(crumb), Some(gretel), Some(reqLabel)) =>
              Set(crumb, gretel) == arcTokens && arcLabel != reqLabel
            case _ => false
          }
      case LabelRightArc(arcLabel) =>
        (
          PreviousLinkCrumbRef(state).headOption,
          PreviousLinkGretelRef(state).headOption, requestedArc.arcLabel
        ) match {
            case (Some(crumb), Some(gretel), Some(reqLabel)) =>
              Set(crumb, gretel) == arcTokens && arcLabel != reqLabel
            case _ => false
          }
      case _ => false
    }
  }
}

/** The ArcHybridForbiddenArcLabelInterpretation handles ForbiddenArcLabel constraints for the
  * arc hybrid system. In other words, it translates these constraints into a function that
  * returns true for any (state, transition) pair that violates the constraint.
  *
  * @param forbiddenArcLabel the forbidden arc label request to consider
  */
case class ArcHybridForbiddenArcLabelInterpretation(
    forbiddenArcLabel: ForbiddenArcLabel
) extends ParsingConstraintInterpretation {

  private val arcTokens: Set[Int] = Set(forbiddenArcLabel.token1, forbiddenArcLabel.token2)

  def applyToParserState(state: TransitionParserState, transition: StateTransition): Boolean = {
    transition match {
      case LabelLeftArc(arcLabel) =>
        (PreviousLinkCrumbRef(state).headOption, PreviousLinkGretelRef(state).headOption) match {
          case (Some(crumb), Some(gretel)) =>
            Set(crumb, gretel) == arcTokens && arcLabel == forbiddenArcLabel.arcLabel
          case _ => false
        }
      case LabelRightArc(arcLabel) =>
        (PreviousLinkCrumbRef(state).headOption, PreviousLinkGretelRef(state).headOption) match {
          case (Some(crumb), Some(gretel)) =>
            Set(crumb, gretel) == arcTokens && arcLabel == forbiddenArcLabel.arcLabel
          case _ => false
        }
      case _ => false
    }
  }
}
