package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ SentenceTransform, AnnotatedSentence, WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, BrownClusters }

/** Factory object for ArcEagerTransitionSystems.
  *
  * @param taggers a sequence of sentence transforms to use on input sentences (e.g. you might
  * want to part-of-speech-tag the tokens.
  */
case class ArcEagerTransitionSystemFactory(
    taggers: Seq[SentenceTransform]
) extends TransitionSystemFactory {

  def buildTransitionSystem(
    marbleBlock: MarbleBlock,
    constraints: Set[TransitionConstraint]
  ): TransitionSystem = {
    new ArcEagerTransitionSystem(marbleBlock, constraints, taggers)
  }
}

/** The ArcEagerTaskIdentifier identifies the ClassificationTask associated with a particular
  * state of the arc-eager transition system.
  */
object ArcEagerTaskIdentifier extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    state match {
      case tpState: TransitionParserState =>
        tpState.parserMode match {
          case DependencyParserModes.TRANSITION =>
            Some(ApplicabilitySignature(
              StateTransition.applicable(ArcEagerShift, Some(state)),
              StateTransition.applicable(ArcEagerReduce, Some(state)),
              StateTransition.applicable(ArcEagerLeftArc('NONE), Some(state)),
              StateTransition.applicable(ArcEagerRightArc('NONE), Some(state))
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

/** The ArcEagerTransitionSystem has four transition operators: Shift, Reduce,
  * RightArc, and LeftArc.
  *
  * The Shift operator pops the next buffer item and pushes it onto the stack.
  *
  * The Reduce operator pops the next stack item.
  *
  * The RightArc operator creates an arc from the top element of the stack to the next buffer item,
  * then performs a Shift operation.
  *
  * The LeftArc operator creates an arc from the next buffer item to the top element
  * of the stack, then performs a Reduce operation.
  *
  */
case class ArcEagerTransitionSystem(
    marbleBlock: MarbleBlock,
    constraints: Set[TransitionConstraint],
    taggers: Seq[SentenceTransform]
) extends DependencyParsingTransitionSystem(marbleBlock, constraints, taggers) {

  @transient
  override val taskIdentifier: TaskIdentifier = ArcEagerTaskIdentifier

  override def computeFeature(state: State): FeatureVector = {
    taskIdentifier(state) map { ident => ident.filenameFriendlyName } match {
      case Some(x) if x.startsWith("dt-") => labelingFeature(state)
      case _ => defaultFeature(state)
    }
  }

  override def systemSpecificInitialState(
    sentence: Sentence
  ): TransitionParserState = {
    new TransitionParserState(Vector(0), 1, Map(0 -> -1), Map(), Map(), sentence, None,
      DependencyParserModes.TRANSITION)
  }

  override def guidedCostFunction(goldObj: Sculpture): Option[StateCostFunction] =
    goldObj match {
      case parse: PolytreeParse =>
        Some(new ArcEagerGuidedCostFunction(parse, this))
      case _ => None
    }

  override def interpretConstraint(
    constraint: TransitionConstraint
  ): ((State, StateTransition) => Boolean) = {

    constraint match {
      case forbiddenArc: ForbiddenEdge =>
        ArcEagerForbiddenArcInterpretation(forbiddenArc)
      case requestedArc: RequestedArc =>
        ArcEagerRequestedArcInterpretation(requestedArc)
      case forbiddenArcLabel: ForbiddenArcLabel =>
        ArcHybridForbiddenArcLabelInterpretation(forbiddenArcLabel)
      case _ => TransitionSystem.trivialConstraint
    }
  }
}

case object ArcEagerTransitionSystem {

  val keywords = WordClusters.commonWords ++ WordClusters.puncWords ++
    WordClusters.stopWords

}

/** The ArcEagerShift operator pops the next buffer item and pushes it onto the stack. */
case object ArcEagerShift extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state.bufferPosition + 1 < state.sentence.size
  }

  override def advanceState(state: TransitionParserState): State = {
    state.copy(
      stack = state.bufferPosition +: state.stack,
      bufferPosition = state.bufferPosition + 1
    )
  }

  override val name: String = "Sh"
}

/** The ArcEagerReduce operator pops the top stack item. */
case object ArcEagerReduce extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    // The fourth clause forces the parser to return a connected graph. By ensuring that the
    // first node with nexus as its breadcrumb stays on the stack until the buffer has been
    // fully emptied, this guarantees that only one node will have the nexus as a breadcrumb.
    // Thus when the nexus goes away, the parse will remain connected.
    state.stack.nonEmpty &&
      state.stack.head != 0 &&
      state.breadcrumb.contains(state.stack.head) &&
      !(state.breadcrumb(state.stack.head) == 0 && !state.bufferIsEmpty)
  }

  override def advanceState(state: TransitionParserState): State = {
    state.copy(stack = state.stack.tail)
  }

  override val name: String = "Re"
}

/** The ArcEagerLeftArc operator creates an arc from the next buffer item to the stack top
  * and then performs a Reduce (see above).
  *
  * @param label the label to attach to the created arc
  */
case class ArcEagerLeftArc(label: Symbol = 'NONE) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state match {
      case tpState: TransitionParserState =>
        (tpState.bufferPosition < tpState.sentence.size) && tpState.stack.nonEmpty &&
          !tpState.breadcrumb.contains(tpState.stack.head)
      case _ => false
    }
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

/** The ArcEagerRightArc operator creates an arc from the stack top to the next buffer item and then
  * performs a Shift (see above).
  *
  * @param label the label to attach to the created arc
  */
case class ArcEagerRightArc(label: Symbol = 'NONE) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    state match {
      case tpState: TransitionParserState =>
        // second conjunction means: if we only have one token left in the buffer, then all
        // stack tokens better already have breadcrumbs if we want to shift
        (tpState.bufferPosition < tpState.sentence.size) && tpState.stack.nonEmpty &&
          (tpState.bufferPosition + 1 < tpState.sentence.size ||
            tpState.stack.forall(tpState.breadcrumb.contains(_)))
      case _ => false
    }
  }

  override def advanceState(state: TransitionParserState): State = {
    state.copy(
      stack = state.bufferPosition +: state.stack,
      bufferPosition = state.bufferPosition + 1,
      breadcrumb = state.breadcrumb + (state.bufferPosition -> state.stack.head),
      arcLabels = state.arcLabels +
      (Set(state.stack.head, state.bufferPosition) -> label),
      previousLink = Some((state.stack.head, state.bufferPosition)),
      parserMode = DependencyParserModes.RIGHTLABEL
    )
  }

  @transient
  override val name: String = s"Rt[${label.name}]"
}

/** The ArcEagerGuidedCostFunction uses a gold parse tree to make deterministic decisions
  * about which transition to apply in any given state. Since the decision is uniquely determined
  * by the gold parse, the returned map will have only a single mapping that assigns zero cost
  * to the correct transition (all other transitions therefore have an implicit cost of infinity).
  *
  * @param parse the gold parse tree
  */
class ArcEagerGuidedCostFunction(
    parse: PolytreeParse,
    override val transitionSystem: TransitionSystem
) extends StateCostFunction {

  private val alteredParse: PolytreeParse = parse.copy(
    arclabels = parse.arclabels.zipWithIndex map {
    case (arcSet, token) =>
      arcSet map {
        case (otherToken, label) =>
          val cposToken = if (parse.breadcrumb(otherToken) == token) {
            otherToken
          } else {
            token
          }
          val cpos = parse.tokens(cposToken).getDeterministicProperty('cpos)
          (otherToken, mergeCposWithLabel(cpos, label))
      }
  }
  )

  private def mergeCposWithLabel(cpos: Symbol, label: Symbol): Symbol = {
    Symbol(s"${label.name}::${cpos.name}")
  }

  override def apply(state: State): Map[StateTransition, Double] = {
    state match {
      case tpState: TransitionParserState =>
        require(tpState.stack.nonEmpty)
        if (tpState.parserMode == DependencyParserModes.LEFTLABEL) {
          val (crumb, gretel) = tpState.previousLink.get
          val arclabel = alteredParse.arcLabelByEndNodes(Set(crumb, gretel))
          Map(LabelLeftArc(arclabel) -> 0)
        } else if (tpState.parserMode == DependencyParserModes.RIGHTLABEL) {
          val (crumb, gretel) = tpState.previousLink.get
          val arclabel = alteredParse.arcLabelByEndNodes(Set(crumb, gretel))
          Map(LabelRightArc(arclabel) -> 0)
        } else if (tpState.bufferIsEmpty) {
          Map(ArcEagerReduce -> 0)
        } else if (alteredParse.breadcrumb(tpState.bufferPosition) == tpState.stack.head) {
          Map(ArcEagerRightArc() -> 0)
        } else if (alteredParse.breadcrumb(tpState.stack.head) == tpState.bufferPosition) {
          Map(ArcEagerLeftArc() -> 0)
        } else if (tpState.stack.tail forall (!alteredParse.areNeighbors(_, tpState.bufferPosition))) {
          // once we shift the next buffer item, we will no longer be able to attach it to anything
          // currently on the stack; therefore only shift if it has no remaining neighbors on the
          // stack (note: the previous checks have already established that it is not neighbors
          // with the stack head, so no need to check that again)
          Map(ArcEagerShift -> 0)
        } else {
          Map(ArcEagerReduce -> 0)
        }
    }
  }
}

/** The ArcEagerForbiddenArcInterpretation handles ForbiddenArc constraints for the arc-eager
  * system. In other words, it translates these constraints into a function that returns true
  * for any (state, transition) pair that violates the constraint.
  *
  * @param forbiddenArc the forbidden arc constraint to consider
  */
case class ArcEagerForbiddenArcInterpretation(
    forbiddenArc: ForbiddenEdge
) extends ParsingConstraintInterpretation {

  def applyToParserState(state: TransitionParserState, transition: StateTransition): Boolean = {
    (StackRef(0)(state).headOption, BufferRef(0)(state).headOption) match {
      case (Some(stackTop), Some(bufferTop)) =>
        transition match {
          case ArcEagerShift => false
          case ArcEagerReduce => false
          case _ =>
            Set(forbiddenArc.token1, forbiddenArc.token2) == Set(stackTop, bufferTop)
        }
      case _ => false
    }
  }
}

/** The ArcEagerRequestedArcInterpretation handles RequestedArc constraints for the arc-eager
  * system. In other words, it translates these constraints into a function that returns true
  * for any (state, transition) pair that violates the constraint.
  *
  * @param requestedArc the requested arc constraint to consider
  */
case class ArcEagerRequestedArcInterpretation(
    requestedArc: RequestedArc
) extends ParsingConstraintInterpretation {

  private val arcTokens: Set[Int] = Set(requestedArc.token1, requestedArc.token2)
  private val tokenA: Int = List(requestedArc.token1, requestedArc.token2).min
  private val tokenB: Int = List(requestedArc.token1, requestedArc.token2).max

  def applyToParserState(state: TransitionParserState, transition: StateTransition): Boolean = {
    (StackRef(0)(state).headOption, BufferRef(0)(state).headOption) match {
      case (Some(stackTop), Some(bufferTop)) =>
        transition match {
          case ArcEagerShift => (bufferTop == tokenB) && !state.areNeighbors(tokenA, tokenB)
          case ArcEagerRightArc(_) =>
            bufferTop == tokenB &&
              !state.areNeighbors(tokenA, tokenB) &&
              stackTop != tokenA
          case ArcEagerReduce => (stackTop == tokenA) && !state.areNeighbors(tokenA, tokenB)
          case ArcEagerLeftArc(_) =>
            stackTop == tokenA &&
              !state.areNeighbors(tokenA, tokenB) &&
              bufferTop != tokenB
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
      case _ => false
    }
  }
}
