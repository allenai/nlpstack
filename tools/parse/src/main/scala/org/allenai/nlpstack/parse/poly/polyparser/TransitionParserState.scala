package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ AnnotatedSentence, Token, NexusToken, Sentence }

import org.allenai.nlpstack.parse.poly.fsm.{
  ConstraintInterpretation,
  Sculpture,
  StateTransition,
  State
}

/** A TransitionParserState captures the current state of a transition-based parser (i.e. it
  * corresponds to a partially constructed PolytreeParse). It includes the following fields:
  * - the `stack` holds the indices of the tokens (note: the index of a token is its index in
  * the `tokens` vector) on the stack. It is a vector of integers. The head of the vector
  * represents the top of the stack.
  * - the `bufferPosition` is an integer representing the index of the token that is currently
  * at the front of the buffer.
  * - `breadcrumb` maps the index of a token to its breadcrumb (see
  * org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse for the definition of breadcrumb).
  * If a token index does not appear as a key in `breadcrumb`, then its breadcrumb has not yet been
  * determined.
  * - `children` maps the index of a token to the indices of its children (in the partially
  * constructed polytree).
  * - `arcLabels` maps a pair of token indices to the label of the arc between them. This
  * presupposes that the two tokens are neighbors in the partially constructed polytree. Note
  * that the pair of token indices is represented as a Set, so order is irrelevant.
  * - `tokens` is the sequence of tokens in the sentence we are trying to parse. This will be
  * invariant for all states of a given parsing process.
  *
  * @param stack the indices of the token indices on the 'stack' (stack.head is the stack top)
  * @param bufferPosition the index of the token at the front of the 'buffer'
  * @param breadcrumb the breadcrumbs of the partially constructed PolytreeParse
  * @param children the children of the partially constructed PolytreeParse
  * @param arcLabels the arc labels of the partially constructed PolytreeParse
  * @param sentence the sentence we want to parse
  */
case class TransitionParserState(val stack: Vector[Int], val bufferPosition: Int,
    val breadcrumb: Map[Int, Int], val children: Map[Int, Set[Int]],
    val arcLabels: Map[Set[Int], Symbol], val sentence: Sentence,
    val previousLink: Option[(Int, Int)] = None, val parserMode: Int = 0) extends State {

  /** Gets the set of gretels of the specified token in the partial parse tree
    * represented by this state.
    *
    * @param token the token (index) of interest
    * @return the set of that token's gretels
    */
  def getGretels(token: Int): Set[Int] = {
    (breadcrumb filter {
      case (gretel, crumb) =>
        crumb == token
    }).keySet
  }

  /** Gets the set of parents of the specified token in the partial parse tree
    * represented by this state.
    *
    * @param token the token (index) of interest
    * @return the set of that token's parents
    */
  def getParents(token: Int): Set[Int] = {
    parents.getOrElse(token, Set())
  }

  /** Maps each node (that has parents) to the set of its parents. */
  private lazy val parents: Map[Int, Set[Int]] = {
    val childParentPairs: Seq[(Int, Int)] = for {
      (parent, childSet) <- children.toSeq
      child <- childSet
    } yield (child, parent)
    (childParentPairs groupBy (_._1)).mapValues {
      case intPairSeq: Seq[(Int, Int)] =>
        (intPairSeq map { _._2 }).toSet
    }
  }

  /** Returns whether the buffer has been exhausted. */
  lazy val bufferIsEmpty: Boolean = (bufferPosition >= sentence.size)

  /** Returns whether this is the final state of a transition parse. */
  lazy val isFinal: Boolean = (stack.size <= 1 && bufferIsEmpty)

  /** Returns whether the two tokens are connected in the parse created thus far. */
  def areNeighbors(token1: Int, token2: Int): Boolean = arcLabels.contains(Set(token1, token2))

  /** Applies the provided sequence of Transitions (in order) to the state.
    *
    * @param transitions a list of Transitions to be applied (in order)
    * @return the new state resulting from the sequence of transitions applied to this state
    */
  def applyTransitionSequence(
    transitions: Seq[TransitionParserState => TransitionParserState]
  ): TransitionParserState = {

    transitions.foldLeft(this) { (state, transition) => transition(state) }
  }

  override def toString: String = {
    (stack map (sentence.tokens(_).word.name)).reverse.mkString(" ") + " ||| " +
      (sentence.tokens.lift(bufferPosition) map { tok => tok.word.name })
  }

  def asSculpture: Option[Sculpture] = {
    if (isFinal) {
      val parseBreadcrumb = ((0 to sentence.size - 1) map { x =>
        breadcrumb(x)
      }).toVector
      val childMap: Map[Int, Set[Int]] = (parseBreadcrumb.zipWithIndex.tail groupBy { _._1 }) map
        { case (key, value) => (key, (value map { _._2 }).toSet) }
      val neighbors: Vector[Set[Int]] = ((0 to (sentence.size - 1)) map
        { i => childMap.getOrElse(i, Set()) + breadcrumb(i) }).toVector
      val parseArcLabels: Vector[Set[(Int, Symbol)]] = for {
        (neighborSet, i) <- neighbors.zipWithIndex
      } yield for {
        neighbor <- neighborSet
        if neighbor >= 0
      } yield (neighbor, arcLabels(Set(i, neighbor)))
      Some(PolytreeParse(
        sentence,
        parseBreadcrumb,
        ((0 to sentence.size - 1) map { x =>
        children.getOrElse(x, Set())
      }).toVector,
        parseArcLabels
      ))
    } else {
      None
    }
  }
}

abstract class TransitionParserStateTransition extends StateTransition {

  override def apply(state: Option[State]): Option[State] = {
    state filter { someState =>
      someState match {
        case tpState: TransitionParserState => satisfiesPreconditions(tpState)
        case _ => false
      }
    } map { someState =>
      someState match {
        case tpState: TransitionParserState =>
          advanceState(tpState)
      }
    }
  }

  def satisfiesPreconditions(state: TransitionParserState): Boolean

  def advanceState(state: TransitionParserState): State
}

/** A ParsingConstraintInterpretation is a ConstraintInterpretation that fires only on
  * TransitionParserState objects.
  */
abstract class ParsingConstraintInterpretation extends ConstraintInterpretation {

  def apply(state: State, transition: StateTransition): Boolean = {
    state match {
      case tpState: TransitionParserState => applyToParserState(tpState, transition)
      case _ => false
    }
  }

  def applyToParserState(state: TransitionParserState, transition: StateTransition): Boolean
}

