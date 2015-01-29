package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters

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
      new TransitionParserState(Vector(0), 1, Map(0 -> -1), Map(), Map(), augmentedSentence)
    }
  }

  override def guidedCostFunction(goldObj: MarbleBlock): Option[StateCostFunction] =
    goldObj match {
      case parse: PolytreeParse =>
        Some(new ArcEagerGuidedCostFunction(parse, this))
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
      new TokenCardinalityFeature(Seq(StackRef(0), StackRef(1), BufferRef(0), BufferRef(1),
        StackGretelsRef(0), StackGretelsRef(1), BreadcrumbRef(0), StackLeftGretelsRef(0),
        StackRightGretelsRef(1), BufferGretelsRef(0))),
      new OfflineTokenFeature(StackRef(0)),
      new OfflineTokenFeature(StackRef(1)),
      new OfflineTokenFeature(BufferRef(0)),
      new OfflineTokenFeature(BufferRef(1)),
      new OfflineTokenFeature(StackGretelsRef(0)),
      new OfflineTokenFeature(StackGretelsRef(1)),
      new OfflineTokenFeature(BufferGretelsRef(0)),
      new OfflineTokenFeature(BreadcrumbRef(0)),
      new TokenTransformFeature(LastRef, Set(KeywordTransform(WordClusters.puncWords))),
      new TokenTransformFeature(FirstRef, Set(TokenPropertyTransform('factorieCpos)))
    )
    new FeatureUnion(features)
  }
}

/** The ArcHybridShift operator pops the next buffer item and pushes it onto the stack. */
case object ArcHybridShift extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    (state.bufferPosition + 1 < state.sentence.size)
  }

  override def advanceState(state: TransitionParserState): State = {
    state.copy(
      stack = state.bufferPosition +: state.stack,
      bufferPosition = state.bufferPosition + 1
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
    ArcEagerLeftArc.applicable(state)
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
  * TODO: NOT DONE
  *
  * @param label the label to attach to the created arc
  */
case class ArcHybridRightArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    ArcEagerRightArc.applicable(state)
  }

  override def advanceState(state: TransitionParserState): State = {
    val stackHeadChildren: Set[Int] =
      state.children.getOrElse(state.stack.head, Set.empty[Int])
    state.copy(
      stack = state.bufferPosition +: state.stack,
      bufferPosition = state.bufferPosition + 1,
      breadcrumb = state.breadcrumb + (state.bufferPosition -> state.stack.head),
      children = state.children +
      (state.stack.head -> (stackHeadChildren + state.bufferPosition)),
      arcLabels = state.arcLabels +
      (Set(state.stack.head, state.bufferPosition) -> label)
    )
  }

  @transient
  override val name: String = s"Rt[${label.name}]"
}

object ArcHybridRightArc {
  def applicable(state: State): Boolean = {
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
}

/** The ArcEagerGuidedCostFunction uses a gold parse tree to make deterministic decisions
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
    state match {
      case tpState: TransitionParserState =>
        require(tpState.stack.nonEmpty)
        if (tpState.bufferIsEmpty) {
          Map(ArcEagerReduce -> 0)
        } else if (parse.breadcrumb(tpState.bufferPosition) == tpState.stack.head) {
          val arcLabel = parse.arcLabelByEndNodes(Set(tpState.stack.head, tpState.bufferPosition))
          //val arcLabel = parse.tokens(tpState.bufferPosition).getProperty('cpos).head
          if (parse.children(tpState.stack.head).contains(tpState.bufferPosition)) {
            Map(ArcEagerRightArc(arcLabel) -> 0)
          } else {
            Map(ArcEagerInvertedRightArc(arcLabel) -> 0)
          }
        } else if (parse.breadcrumb(tpState.stack.head) == tpState.bufferPosition) {
          val arcLabel = parse.arcLabelByEndNodes(Set(tpState.stack.head, tpState.bufferPosition))
          //val arcLabel = parse.tokens(tpState.stack.head).getProperty('cpos).head
          if (parse.children(tpState.bufferPosition).contains(tpState.stack.head)) {
            Map(ArcEagerLeftArc(arcLabel) -> 0)
          } else {
            Map(ArcEagerInvertedLeftArc(arcLabel) -> 0)
          }
        } else if (tpState.stack.tail forall (!parse.areNeighbors(_, tpState.bufferPosition))) {
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

