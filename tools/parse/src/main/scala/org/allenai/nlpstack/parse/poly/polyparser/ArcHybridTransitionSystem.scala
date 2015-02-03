package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters

object ArcHybridModes {
  val TRANSITION: Int = 0
  val LEFTLABEL: Int = 1
  val RIGHTLABEL: Int = 2
}

/** The HybridApplicabilitySignatureIdentifier identifies the ClassificationTask of a parser state
  * according to the state's applicability signature (for an ArcHybrid transition system).
  */
object HybridApplicabilitySignatureIdentifier extends TaskIdentifier {
  override def apply(state: State): Option[ClassificationTask] = {
    Some(ApplicabilitySignature(
      StateTransition.applicable(ArcHybridShift, Some(state)),
      false,
      StateTransition.applicable(ArcHybridLeftArc('NONE), Some(state)),
      StateTransition.applicable(ArcHybridRightArc('NONE), Some(state))
    ))
  }
}

object HybridTaskIdentifier extends TaskIdentifier {
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
            Some(SimpleTask("leftlabel"))
          case ArcHybridModes.RIGHTLABEL =>
            Some(SimpleTask("rightlabel"))
          //Some(ApplicabilitySignature(false, false, false, false))
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
  * @param feature the features to use
  * @param brownClusters an optional set of Brown clusters to use
  */
case class ArcHybridTransitionSystem(
    feature: StateFeature = ArcHybridTransitionSystem.defaultFeature,
    brownClusters: Seq[BrownClusters] = Seq()
) extends TransitionSystem {

  @transient
  override val taskIdentifier: TaskIdentifier = HybridTaskIdentifier

  @transient private val tokenFeatureTagger = new TokenFeatureTagger(Seq(
    TokenPositionFeature,
    TokenPropertyFeature('factorieCpos),
    TokenPropertyFeature('factoriePos),
    TokenPropertyFeature('brown0),
    KeywordFeature(ArcHybridTransitionSystem.keywords)
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
    sentence map { sent =>
      val taggedSentence = sent.taggedWithFactorie
        .taggedWithBrownClusters(brownClusters)
      val augmentedSentence = tokenFeatureTagger.tag(taggedSentence)
      new TransitionParserState(Vector(), 1, Map(0 -> -1), Map(), Map(), augmentedSentence,
        None, ArcHybridModes.TRANSITION)
    }
  }

  override def guidedCostFunction(goldObj: MarbleBlock): Option[StateCostFunction] =
    goldObj match {
      case parse: PolytreeParse =>
        Some(new ArcHybridGuidedCostFunction(parse, this))
      case _ => None
    }

  override def toSculpture(state: State): Option[Sculpture] = {
    state match {
      case tpState: TransitionParserState =>
        tpState.asSculpture
      case _ =>
        None
    }
  }

  override def interpretConstraint(constraint: TransitionConstraint): ((State, StateTransition) => Boolean) = {

    TransitionSystem.trivialConstraint
  }
}

case object ArcHybridTransitionSystem {

  val keywords = WordClusters.commonWords ++ WordClusters.puncWords ++
    WordClusters.stopWords

  val defaultFeature = constructDefaultFeature(keywords)

  def constructDefaultFeature(keywords: Set[Symbol]): StateFeature = {
    val features = List(
      new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), StackRef(2), BufferRef(0), BufferRef(1),
        PreviousLinkCrumbRef, PreviousLinkGretelRef,
        StackGretelsRef(0), StackGretelsRef(1), StackLeftGretelsRef(0),
        StackRightGretelsRef(0), BufferGretelsRef(0))),
      new OfflineTokenFeature(StackRef(0)),
      new OfflineTokenFeature(StackRef(1)),
      new OfflineTokenFeature(BufferRef(0)),
      new OfflineTokenFeature(BufferRef(1)),
      new OfflineTokenFeature(PreviousLinkCrumbRef),
      new OfflineTokenFeature(PreviousLinkGretelRef),
      new OfflineTokenFeature(StackGretelsRef(0)),
      new OfflineTokenFeature(StackGretelsRef(1)),
      new OfflineTokenFeature(BufferGretelsRef(0)),
      //PreviousLinkDirection,
      new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords))),
      new TokenTransformFeature(FirstRef, Set(TokenPropertyTransform('factorieCpos)))
    )
    new FeatureUnion(features)
  }
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
