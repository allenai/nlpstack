package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, WordClusters }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, BrownClusters }

/** A struct contains the "modes" of an arc-hybrid transition parser.
  *
  * Specifically, the mode tells you what the next expected action is.
  */
object ArcHybridModes {
  val TRANSITION: Int = 0
  val LEFTLABEL: Int = 1
  val RIGHTLABEL: Int = 2
}

/** The ArcHybridTaskIdentifier identifies the ClassificationTask associated with a particular
  * state of the transition system.
  */
object ArcHybridTaskIdentifier extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    state match {
      case tpState: TransitionParserState =>
        tpState.parserMode match {
          case ArcHybridModes.TRANSITION =>
            Some(ApplicabilitySignature(
              StateTransition.applicable(ArcHybridShift, Some(state)),
              false,
              StateTransition.applicable(ArcHybridLeftArc('NONE), Some(state)),
              StateTransition.applicable(ArcHybridRightArc('NONE), Some(state))
            ))
          case ArcHybridModes.LEFTLABEL =>
            Some(SimpleTask("dt-leftlabel"))
          case ArcHybridModes.RIGHTLABEL =>
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
  * @param brownClusters an optional set of Brown clusters to use
  */
case class ArcHybridTransitionSystem(
    brownClusters: Seq[BrownClusters] = Seq()
) extends DependencyParsingTransitionSystem(brownClusters) {

  @transient
  override val taskIdentifier: TaskIdentifier = ArcHybridTaskIdentifier

  override def computeFeature(state: State): FeatureVector = {
    (taskIdentifier(state) map { ident => ident.filenameFriendlyName }) match {
      case Some(x) if x.startsWith("dt-") => ArcHybridTransitionSystem.labelingFeature(state)
      case _ => ArcHybridTransitionSystem.transitionFeature(state)
    }
  }

  override def systemSpecificInitialState(
    annotatedSentence: AnnotatedSentence
  ): TransitionParserState = {
    new TransitionParserState(Vector(), 1, Map(0 -> -1), Map(), Map(), annotatedSentence,
      None, ArcHybridModes.TRANSITION)
  }

  override def guidedCostFunction(goldObj: MarbleBlock): Option[StateCostFunction] =
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

case object ArcHybridTransitionSystem {

  val labelingFeature = FeatureUnion(List(
    new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), StackRef(2), BufferRef(0),
      BufferRef(1), PreviousLinkCrumbRef, PreviousLinkGretelRef, PreviousLinkCrumbGretelRef,
      PreviousLinkGrandgretelRef, StackGretelsRef(0), StackGretelsRef(1), StackLeftGretelsRef(0),
      StackRightGretelsRef(0), BufferGretelsRef(0))),
    new OfflineTokenFeature(StackRef(0)),
    new OfflineTokenFeature(StackRef(1)),
    new OfflineTokenFeature(StackRef(2)),
    new OfflineTokenFeature(BufferRef(0)),
    new OfflineTokenFeature(BufferRef(1)),
    new OfflineTokenFeature(PreviousLinkCrumbRef),
    new OfflineTokenFeature(PreviousLinkGretelRef),
    new OfflineTokenFeature(PreviousLinkCrumbGretelRef),
    new OfflineTokenFeature(PreviousLinkGrandgretelRef),
    new OfflineTokenFeature(StackGretelsRef(0)),
    new OfflineTokenFeature(StackLeftGretelsRef(0)),
    new OfflineTokenFeature(StackRightGretelsRef(0)),
    new OfflineTokenFeature(StackGretelsRef(1)),
    new OfflineTokenFeature(BufferGretelsRef(0)),
    new OfflineTokenFeature(LastRef),
    new OfflineTokenFeature(FirstRef)
  ))

  val transitionFeature = FeatureUnion(List(
    new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), StackRef(2), BufferRef(0),
      BufferRef(1), StackGretelsRef(0), StackGretelsRef(1), StackLeftGretelsRef(0),
      StackRightGretelsRef(0), BufferGretelsRef(0))),
    new OfflineTokenFeature(StackRef(0)),
    new OfflineTokenFeature(StackRef(1)),
    new OfflineTokenFeature(StackRef(2)),
    new OfflineTokenFeature(BufferRef(0)),
    new OfflineTokenFeature(BufferRef(1)),
    new OfflineTokenFeature(StackGretelsRef(0)),
    new OfflineTokenFeature(StackLeftGretelsRef(0)),
    new OfflineTokenFeature(StackRightGretelsRef(0)),
    new OfflineTokenFeature(StackGretelsRef(1)),
    new OfflineTokenFeature(BufferGretelsRef(0)),
    new OfflineTokenFeature(LastRef),
    new OfflineTokenFeature(FirstRef)
  ))
}

/** The ArcHybridShift operator pops the next buffer item and pushes it onto the stack. */
case object ArcHybridShift extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == ArcHybridModes.TRANSITION &&
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
    (state.parserMode == ArcHybridModes.TRANSITION) && ArcHybridLeftArc.applicable(state)
  }

  override def advanceState(state: TransitionParserState): State = {
    val bufferPositionChildren: Set[Int] =
      state.children.getOrElse(state.bufferPosition, Set.empty[Int])
    state.copy(
      stack = state.stack.tail,
      breadcrumb = state.breadcrumb + (state.stack.head -> state.bufferPosition),
      children = state.children +
      (state.bufferPosition -> (bufferPositionChildren + state.stack.head)),
      arcLabels = state.arcLabels +
      (Set(state.stack.head, state.bufferPosition) -> label),
      previousLink = Some((state.bufferPosition, state.stack.head)),
      parserMode = ArcHybridModes.LEFTLABEL
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
    (state.parserMode == ArcHybridModes.TRANSITION) && ArcHybridRightArc.applicable(state)
  }

  override def advanceState(state: TransitionParserState): State = {
    val stackFirst = state.stack.head
    val stackSecond = state.stack.tail.head
    val stackSecondChildren: Set[Int] =
      state.children.getOrElse(stackSecond, Set.empty[Int])
    val result = state.copy(
      stack = state.stack.tail,
      breadcrumb = state.breadcrumb + (stackFirst -> stackSecond),
      children = state.children +
      (stackSecond -> (stackSecondChildren + stackFirst)),
      arcLabels = state.arcLabels +
      (Set(stackFirst, stackSecond) -> label),
      previousLink = Some((stackSecond, stackFirst)),
      parserMode = ArcHybridModes.RIGHTLABEL
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

/** The LabelArc operator labels the most recently created arc. */
case class LeftLabelArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == ArcHybridModes.LEFTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    state.previousLink match {
      case Some((crumb, gretel)) =>
        state.copy(
          arcLabels = state.arcLabels +
          (Set(crumb, gretel) -> label),
          parserMode = ArcHybridModes.TRANSITION
        )
      case _ => ???
    }
  }

  @transient
  override val name: String = s"LtLbl[${label.name}]"
}

/** The LabelArc operator labels the most recently created arc. */
case class RightLabelArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.parserMode == ArcHybridModes.RIGHTLABEL
  }

  override def advanceState(state: TransitionParserState): State = {
    require(state.previousLink != None, s"Cannot proceed without an arc to label: $state")
    state.previousLink match {
      case Some((crumb, gretel)) =>
        state.copy(
          arcLabels = state.arcLabels +
          (Set(crumb, gretel) -> label),
          parserMode = ArcHybridModes.TRANSITION
        )
      case _ => ???
    }
  }

  @transient
  override val name: String = s"RtLbl[${label.name}]"
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
        if (tpState.parserMode == ArcHybridModes.LEFTLABEL) {
          val (crumb, gretel) = tpState.previousLink.get
          val arclabel = parse.arcLabelByEndNodes(Set(crumb, gretel))
          Map(LeftLabelArc(arclabel) -> 0)
        } else if (tpState.parserMode == ArcHybridModes.RIGHTLABEL) {
          val (crumb, gretel) = tpState.previousLink.get
          val arclabel = parse.arcLabelByEndNodes(Set(crumb, gretel))
          Map(RightLabelArc(arclabel) -> 0)
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
      case LeftLabelArc(arcLabel) =>
        (
          PreviousLinkCrumbRef(state).headOption,
          PreviousLinkGretelRef(state).headOption, requestedArc.arcLabel
        ) match {
            case (Some(crumb), Some(gretel), Some(reqLabel)) =>
              Set(crumb, gretel) == arcTokens && arcLabel != reqLabel
            case _ => false
          }
      case RightLabelArc(arcLabel) =>
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
      case LeftLabelArc(arcLabel) =>
        (PreviousLinkCrumbRef(state).headOption, PreviousLinkGretelRef(state).headOption) match {
          case (Some(crumb), Some(gretel)) =>
            Set(crumb, gretel) == arcTokens && arcLabel == forbiddenArcLabel.arcLabel
          case _ => false
        }
      case RightLabelArc(arcLabel) =>
        (PreviousLinkCrumbRef(state).headOption, PreviousLinkGretelRef(state).headOption) match {
          case (Some(crumb), Some(gretel)) =>
            Set(crumb, gretel) == arcTokens && arcLabel == forbiddenArcLabel.arcLabel
          case _ => false
        }
      case _ => false
    }
  }
}
