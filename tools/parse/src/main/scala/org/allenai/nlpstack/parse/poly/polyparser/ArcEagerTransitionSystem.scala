package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ WordClusters, Sentence }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, BrownClusters }

case class ArcEagerTransitionSystem(
    feature: StateFeature = ArcEagerTransitionSystem.defaultFeature,
    brownClusters: Seq[BrownClusters] = Seq(),
    taskIdentifier: TaskIdentifier = ApplicabilitySignatureIdentifier
) extends TransitionSystem {

  override def computeFeature(state: State): FeatureVector = feature(state)

  @transient private val tokenFeatureTagger = new TokenFeatureTagger(Seq(
    TokenPositionFeature,
    TokenPropertyFeature('factorieCpos),
    TokenPropertyFeature('factoriePos),
    TokenPropertyFeature('brown0),
    //TokenPropertyFeature('lexical),
    //SuffixFeature(WordClusters.suffixes.toSeq),
    KeywordFeature(ArcEagerTransitionSystem.keywords)
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
        //.taggedWithLexicalProperties
        .taggedWithBrownClusters(brownClusters)

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

      val augmentedSentence = tokenFeatureTagger.tag(overriddenSentence)
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

  override def interpretConstraint(
    constraint: TransitionConstraint
  ): ((State, StateTransition) => Boolean) = {

    constraint match {
      case ForbiddenEdge(token1, token2) => { (state: State, transition: StateTransition) =>
        state match {
          case tpState: TransitionParserState =>
            StackRef(0)(tpState).headOption match {
              case Some(stackTop) => BufferRef(0)(tpState).headOption match {
                case Some(bufferTop) => transition match {
                  case ArcEagerShift => false
                  case ArcEagerReduce => false
                  case _ => {
                    Set(token1, token2) == Set(stackTop, bufferTop)
                  }
                }
                case None => false
              }
              case None => false
            }
          case _ => false
        }
      }
      case requestedArc: RequestedArc =>
        ArcEagerTransitionSystem.requestedArcInterpretation(requestedArc)
      case forbiddenArcLabel: ForbiddenArcLabel =>
        ArcEagerTransitionSystem.forbiddenArcLabelInterpretation(forbiddenArcLabel)
      case _ => TransitionSystem.trivialConstraint
    }
  }
}

case object ArcEagerTransitionSystem {

  def requestedArcInterpretation(
    requestedArc: RequestedArc
  ): ((State, StateTransition) => Boolean) = {

    val tokenA: Int = List(requestedArc.token1, requestedArc.token2).min
    val tokenB: Int = List(requestedArc.token1, requestedArc.token2).max
    (state: State, transition: StateTransition) => {
      state match {
        case tpState: TransitionParserState =>
          (StackRef(0)(tpState).headOption, BufferRef(0)(tpState).headOption) match {
            case (Some(stackTop), Some(bufferTop)) =>
              transition match {
                case ArcEagerShift => (bufferTop == tokenB) && !tpState.areNeighbors(tokenA, tokenB)
                case ArcEagerRightArc(alabel) =>
                  (bufferTop == tokenB &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    stackTop != tokenA) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      requestedArc.arcLabel != None &&
                      requestedArc.arcLabel.get != alabel)
                case ArcEagerInvertedRightArc(alabel) =>
                  (bufferTop == tokenB &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    stackTop != tokenA) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      requestedArc.arcLabel != None &&
                      requestedArc.arcLabel.get != alabel)
                case ArcEagerReduce => (stackTop == tokenA) && !tpState.areNeighbors(tokenA, tokenB)
                case ArcEagerLeftArc(alabel) =>
                  (stackTop == tokenA &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    bufferTop != tokenB) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      requestedArc.arcLabel != None &&
                      requestedArc.arcLabel.get != alabel)
                case ArcEagerInvertedLeftArc(alabel) =>
                  (stackTop == tokenA &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    bufferTop != tokenB) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      requestedArc.arcLabel != None &&
                      requestedArc.arcLabel.get != alabel)
                case _ => false
              }
            case _ => false
          }
        case _ => false
      }
    }
  }

  def forbiddenArcLabelInterpretation(
    forbiddenArcLabel: ForbiddenArcLabel
  ): ((State, StateTransition) => Boolean) = {

    val tokenA: Int = List(forbiddenArcLabel.token1, forbiddenArcLabel.token2).min
    val tokenB: Int = List(forbiddenArcLabel.token1, forbiddenArcLabel.token2).max
    (state: State, transition: StateTransition) => {
      state match {
        case tpState: TransitionParserState =>
          (StackRef(0)(tpState).headOption, BufferRef(0)(tpState).headOption) match {
            case (Some(stackTop), Some(bufferTop)) =>
              transition match {
                case ArcEagerShift => (bufferTop == tokenB) && !tpState.areNeighbors(tokenA, tokenB)
                case ArcEagerRightArc(alabel) =>
                  (bufferTop == tokenB &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    stackTop != tokenA) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      forbiddenArcLabel.arcLabel == alabel)
                case ArcEagerInvertedRightArc(alabel) =>
                  (bufferTop == tokenB &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    stackTop != tokenA) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      forbiddenArcLabel.arcLabel == alabel)
                case ArcEagerReduce => (stackTop == tokenA) && !tpState.areNeighbors(tokenA, tokenB)
                case ArcEagerLeftArc(alabel) =>
                  (stackTop == tokenA &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    bufferTop != tokenB) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      forbiddenArcLabel.arcLabel == alabel)
                case ArcEagerInvertedLeftArc(alabel) =>
                  (stackTop == tokenA &&
                    !tpState.areNeighbors(tokenA, tokenB) &&
                    bufferTop != tokenB) ||
                    (stackTop == tokenA &&
                      bufferTop == tokenB &&
                      forbiddenArcLabel.arcLabel == alabel)
                case _ => false
              }
            case _ => false
          }
        case _ => false
      }
    }
  }

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

/** The ArcEagerShift operator pops the next buffer item and pushes it onto the stack. */
case object ArcEagerShift extends TransitionParserStateTransition {

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
case class ArcEagerLeftArc(val label: Symbol) extends TransitionParserStateTransition {

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

object ArcEagerLeftArc {
  def applicable(state: State): Boolean = {
    state match {
      case tpState: TransitionParserState =>
        (tpState.bufferPosition < tpState.sentence.size) && tpState.stack.nonEmpty &&
          !tpState.breadcrumb.contains(tpState.stack.head)
      case _ => false
    }
  }
}

/** The ArcEagerRightArc operator creates an arc from the stack top to the next buffer item and then
  * performs a Shift (see above).
  *
  * @param label the label to attach to the created arc
  */
case class ArcEagerRightArc(val label: Symbol) extends TransitionParserStateTransition {

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

object ArcEagerRightArc {
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

/** The ArcEagerInvertedLeftArc operator creates an inverse arc from the next buffer item
  * to the stack top and then performs a Reduce (see above).
  *
  * @param label the label to attach to the created arc
  */
case class ArcEagerInvertedLeftArc(val label: Symbol) extends TransitionParserStateTransition {

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    ArcEagerLeftArc.applicable(state)
  }

  override def advanceState(state: TransitionParserState): State = {
    val stackHeadChildren: Set[Int] =
      state.children.getOrElse(state.stack.head, Set.empty[Int])
    state.copy(
      stack = state.stack.tail,
      breadcrumb = state.breadcrumb + (state.stack.head -> state.bufferPosition),
      children = state.children +
      (state.stack.head -> (stackHeadChildren + state.bufferPosition)),
      arcLabels = state.arcLabels +
      (Set(state.stack.head, state.bufferPosition) -> label)
    )
  }

  @transient
  override val name: String = s"Lx[${label.name}]"
}

/** The ArcEagerInvertedRightArc operator creates an inverse arc from the stack top to
  * the next buffer item and then performs a Shift (see above).
  *
  * @param label the label to attach to the created arc
  */
case class ArcEagerInvertedRightArc(val label: Symbol) extends TransitionParserStateTransition {

  override def apply(state: Option[State]): Option[State] = {
    state filter { someState =>
      someState match {
        case tpState: TransitionParserState if ArcEagerRightArc.applicable(tpState) => true
        case _ => false
      }
    } map { someState =>
      someState match {
        case tpState: TransitionParserState =>
          val bufferPositionChildren: Set[Int] =
            tpState.children.getOrElse(tpState.bufferPosition, Set.empty[Int])
          tpState.copy(
            stack = tpState.bufferPosition +: tpState.stack,
            bufferPosition = tpState.bufferPosition + 1,
            breadcrumb = tpState.breadcrumb + (tpState.bufferPosition -> tpState.stack.head),
            children = tpState.children +
            (tpState.bufferPosition -> (bufferPositionChildren + tpState.stack.head)),
            arcLabels = tpState.arcLabels +
            (Set(tpState.stack.head, tpState.bufferPosition) -> label)
          )
      }
    }
  }

  override def satisfiesPreconditions(state: TransitionParserState): Boolean = {
    ArcEagerRightArc.applicable(state)
  }

  override def advanceState(state: TransitionParserState): State = {
    val bufferPositionChildren: Set[Int] =
      state.children.getOrElse(state.bufferPosition, Set.empty[Int])
    state.copy(
      stack = state.bufferPosition +: state.stack,
      bufferPosition = state.bufferPosition + 1,
      breadcrumb = state.breadcrumb + (state.bufferPosition -> state.stack.head),
      children = state.children +
      (state.bufferPosition -> (bufferPositionChildren + state.stack.head)),
      arcLabels = state.arcLabels +
      (Set(state.stack.head, state.bufferPosition) -> label)
    )
  }

  @transient
  override val name: String = s"Rx[${label.name}]"
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

