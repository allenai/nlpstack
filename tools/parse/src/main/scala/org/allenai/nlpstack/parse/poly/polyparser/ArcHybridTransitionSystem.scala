package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters

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
      new TransitionParserState(Vector(), 1, Map(0 -> -1), Map(), Map(), augmentedSentence)
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
        StackGretelsRef(0), StackGretelsRef(1), StackLeftGretelsRef(0),
        StackRightGretelsRef(0), BufferGretelsRef(0))),
      new OfflineTokenFeature(StackRef(0)),
      new OfflineTokenFeature(StackRef(1)),
      new OfflineTokenFeature(BufferRef(0)),
      new OfflineTokenFeature(BufferRef(1)),
      new OfflineTokenFeature(StackGretelsRef(0)),
      new OfflineTokenFeature(StackGretelsRef(1)),
      new OfflineTokenFeature(BufferGretelsRef(0)),
      new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords))),
      new TokenTransformFeature(FirstRef, Set(TokenPropertyTransform('factorieCpos)))
    )
    new FeatureUnion(features)
  }
}

/** The ArcHybridShift operator pops the next buffer item and pushes it onto the stack. */
case object ArcHybridShift extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
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
case class ArcHybridLeftArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    ArcHybridLeftArc.applicable(state)
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
      (Set(state.stack.head, state.bufferPosition) -> label)
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
case class ArcHybridRightArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    ArcHybridRightArc.applicable(state)
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
      (Set(stackFirst, stackSecond) -> label)
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
        if (tpState.stack.isEmpty) {
          Map(ArcHybridShift -> 0)
        } else if (tpState.bufferIsEmpty) {
          Map()
        } else {
          val stackFirst = tpState.stack.head
          val bufferFirst = tpState.bufferPosition
          if (parse.breadcrumb(stackFirst) == bufferFirst) {
            val arclabel = parse.arcLabelByEndNodes(Set(stackFirst, bufferFirst))
            Map(ArcHybridLeftArc(arclabel) -> 0)
          } else if (tpState.stack.size < 2) {
            Map(ArcHybridShift -> 0)
          } else {
            val stackSecond = tpState.stack.tail.head
            if (parse.breadcrumb(stackFirst) == stackSecond &&
              parse.getGretels(stackFirst) == tpState.getGretels(stackFirst)) {
              val arclabel = parse.arcLabelByEndNodes(Set(stackFirst, stackSecond))
              Map(ArcHybridRightArc(arclabel) -> 0)
            } else {
              Map(ArcHybridShift -> 0)
            }
          }
        }
    }
    result
  }
}
