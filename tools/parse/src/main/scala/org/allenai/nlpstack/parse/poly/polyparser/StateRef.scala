package org.allenai.nlpstack.parse.poly.polyparser

import reming.DefaultJsonProtocol._

/** A StateRef allows you to figure out the token that corresponds to a particular aspect of a
  * TransitionParserState.
  *
  * For instance, we may want to know what token is at the top of the stack for a given state.
  * Applying StackRef(0) to the state will return the index of the token.
  * More accurately, a set is returned, which will be empty if the StateRef refers
  * to a non-existent element of the state. For instance, applying StackRef(3) to a state whose
  * stack has 3 or fewer elements will return the empty set.
  *
  * This set of classes is used primarily to facilitate feature creation (e.g. see
  * StateRefFeature).
  *
  */
sealed abstract class StateRef
    extends (TransitionParserState => Seq[Int]) {

  /** Provides a symbolic representation of the StateRef, used for creating feature names. */
  def name: Symbol
}

object StateRef {
  private implicit val lastRefFormat = jsonFormat0(() => LastRef)
  private implicit val firstRefFormat = jsonFormat0(() => FirstRef)
  private implicit val previousLinkCrumbRefFormat = jsonFormat0(() => PreviousLinkCrumbRef)
  private implicit val previousLinkCrumbGretelRefFormat =
    jsonFormat0(() => PreviousLinkCrumbGretelRef)
  private implicit val previousLinkGretelRefFormat = jsonFormat0(() => PreviousLinkGretelRef)
  private implicit val previousLinkGrandgretelRefFormat =
    jsonFormat0(() => PreviousLinkGrandgretelRef)
  private implicit val stackRefFormat = jsonFormat1(StackRef.apply)
  private implicit val bufferRefFormat = jsonFormat1(BufferRef.apply)
  private implicit val breadcrumbRefFormat = jsonFormat1(BreadcrumbRef.apply)
  private implicit val stackChildrenRefFormat = jsonFormat1(StackChildrenRef.apply)
  private implicit val stackChildRefFormat = jsonFormat2(StackChildRef.apply)
  private implicit val bufferChildrenRefFormat = jsonFormat1(BufferChildrenRef.apply)
  private implicit val bufferChildRefFormat = jsonFormat2(BufferChildRef.apply)
  private implicit val stackParentsRefFormat = jsonFormat1(StackParentsRef.apply)
  private implicit val stackParentRefFormat = jsonFormat2(StackParentRef.apply)
  private implicit val bufferParentsRefFormat = jsonFormat1(BufferParentsRef.apply)
  private implicit val bufferParentRefFormat = jsonFormat2(BufferParentRef.apply)
  private implicit val stackGretelsRefFormat = jsonFormat1(StackGretelsRef.apply)
  private implicit val stackLeftGretelsRefFormat = jsonFormat1(StackLeftGretelsRef.apply)
  private implicit val stackRightGretelsRefFormat = jsonFormat1(StackRightGretelsRef.apply)
  private implicit val bufferGretelsRefFormat = jsonFormat1(BufferGretelsRef.apply)
  private implicit val bufferLeftGretelsRefFormat = jsonFormat1(BufferLeftGretelsRef.apply)
  private implicit val bufferRightGretelsRefFormat = jsonFormat1(BufferRightGretelsRef.apply)

  implicit val stateRefJsonFormat = parentFormat[StateRef](
    childFormat[LastRef.type, StateRef],
    childFormat[FirstRef.type, StateRef],
    childFormat[PreviousLinkCrumbRef.type, StateRef],
    childFormat[PreviousLinkGretelRef.type, StateRef],
    childFormat[PreviousLinkCrumbGretelRef.type, StateRef],
    childFormat[PreviousLinkGrandgretelRef.type, StateRef],
    childFormat[StackRef, StateRef],
    childFormat[BufferRef, StateRef],
    childFormat[BreadcrumbRef, StateRef],
    childFormat[StackChildrenRef, StateRef],
    childFormat[StackChildRef, StateRef],
    childFormat[BufferChildrenRef, StateRef],
    childFormat[BufferChildRef, StateRef],
    childFormat[StackParentsRef, StateRef],
    childFormat[BufferParentsRef, StateRef],
    childFormat[BufferParentRef, StateRef],
    childFormat[StackGretelsRef, StateRef],
    childFormat[StackLeftGretelsRef, StateRef],
    childFormat[StackRightGretelsRef, StateRef],
    childFormat[BufferGretelsRef, StateRef],
    childFormat[BufferLeftGretelsRef, StateRef],
    childFormat[BufferRightGretelsRef, StateRef]
  )
}

/** A StackRef is a StateRef (see above) whose apply operation returns the `index`th element of
  * the stack, if it exists.
  *
  * @param index the desired stack element, counting from 0 (i.e. 0 is the stack top)
  */
case class StackRef(val index: Int) extends StateRef {
  require(index >= 0)

  override def apply(state: TransitionParserState): Seq[Int] = {
    Seq(state.stack.lift(index)).flatten
  }

  @transient
  override val name: Symbol = Symbol("s" + index)
}

/** A BufferRef is a StateRef (see above) whose apply operation returns the `index`th element of
  * the buffer, if it exists.
  *
  * @param index the desired buffer element, counting from 0 (i.e. 0 is the front of the buffer)
  */
case class BufferRef(val index: Int) extends StateRef {
  require(index >= 0)

  override def apply(state: TransitionParserState): Seq[Int] = {
    val result = state.bufferPosition + index
    if (result < state.sentence.tokens.size) {
      Seq(result)
    } else {
      Seq()
    }
  }

  @transient
  override val name: Symbol = Symbol("b" + index)
}

/** A FirstRef is a StateRef (see above) whose apply operation returns the first element of
  * the sentence.
  */
case object FirstRef extends StateRef {

  override def apply(state: TransitionParserState): Seq[Int] = {
    if (state.sentence.tokens.size > 1) {
      Seq(1)
    } else {
      Seq()
    }
  }

  @transient
  override val name: Symbol = Symbol("first")
}

/** A LastRef is a StateRef (see above) whose apply operation returns the final element of
  * the sentence.
  */
case object LastRef extends StateRef {

  override def apply(state: TransitionParserState): Seq[Int] = {
    Seq(state.sentence.tokens.size - 1)
  }

  @transient
  override val name: Symbol = Symbol("last")
}

sealed abstract class TokenNeighbors
    extends ((TransitionParserState, Int) => Set[Int]) {

  /** Provides a symbolic representation of the TokenNeighborhood,
    * used for creating feature names.
    */
  def name: Symbol
}

case object TokenChildren extends TokenNeighbors {
  override def apply(state: TransitionParserState, token: Int): Set[Int] = {
    state.children.getOrElse(token, Set())
  }

  @transient override val name: Symbol = Symbol("c")
}

case class TokenChild(childIndex: Int) extends TokenNeighbors {
  require(childIndex >= 0, "the child index of a TokenChild must be a nonnegative integer")

  override def apply(state: TransitionParserState, token: Int): Set[Int] = {
    Set(TokenChildren(state, token).toSeq.sorted.lift(childIndex)).flatten
  }

  @transient override val name: Symbol = Symbol(s"c$childIndex")
}

case object TokenParents extends TokenNeighbors {
  override def apply(state: TransitionParserState, token: Int): Set[Int] = {
    state.getParents(token).toSet
  }

  @transient override val name: Symbol = Symbol("p")
}

case class TokenParent(parentIndex: Int) extends TokenNeighbors {
  require(parentIndex >= 0, "the parent index of a TokenParent must be a nonnegative integer")

  override def apply(state: TransitionParserState, token: Int): Set[Int] = {
    Set(TokenParents(state, token).toSeq.sorted.lift(parentIndex)).flatten
  }

  @transient override val name: Symbol = Symbol(s"p$parentIndex")
}

case object TokenGretels extends TokenNeighbors {
  override def apply(state: TransitionParserState, token: Int): Set[Int] = {
    state.getGretels(token)
  }

  @transient override val name: Symbol = Symbol("g")
}

case class TokenGretel(gretelIndex: Int) extends TokenNeighbors {
  require(gretelIndex >= 0, "the gretel index of a TokenGretel must be a nonnegative integer")

  override def apply(state: TransitionParserState, token: Int): Set[Int] = {
    Set(TokenGretels(state, token).toSeq.sorted.lift(gretelIndex)).flatten
  }

  @transient override val name: Symbol = Symbol(s"g$gretelIndex")
}

case object TokenCrumb extends TokenNeighbors {
  override def apply(state: TransitionParserState, token: Int): Set[Int] = {
    Set(state.breadcrumb.get(token)).flatten -- Set(-1)
  }

  @transient override val name: Symbol = Symbol("p")
}

// TODO: currently cannot be serialized
case class TransitiveRef(stateRef: StateRef, neighbors: Seq[TokenNeighbors]) extends StateRef {
  override def apply(state: TransitionParserState): Seq[Int] = {
    (neighbors.foldLeft(stateRef(state).toSet)((tokens, neighbor) =>
      tokens.flatMap(tok => neighbor(state, tok)))).toSeq.sorted
  }

  @transient override val name: Symbol =
    Symbol(s"${stateRef.name.name}${(neighbors map { _.name.name }).mkString("")}")
}

// ************
// Beyond this line, these StateRefs should be preserved temporarily for legacy models, but are
// not needed in the future and should be regarded as deprecated.

case class StackChildrenRef(stackIndex: Int) extends StateRef {
  require(stackIndex >= 0, "the index of a StackChildrenRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    StackRef(stackIndex)(state) flatMap { nodeIndex =>
      state.children.getOrElse(nodeIndex, Seq()).toSeq.sorted
    }
  }

  @transient override val name: Symbol = Symbol(s"s${stackIndex}c")
}

case class StackChildRef(val stackIndex: Int, val childIndex: Int) extends StateRef {
  require(stackIndex >= 0, "the stack index of a StackChildRef must be a nonnegative integer")
  require(childIndex >= 0, "the child index of a StackChildRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    Seq(StackChildrenRef(stackIndex)(state).lift(childIndex)).flatten
  }

  @transient override val name: Symbol = Symbol(s"s${stackIndex}c${childIndex}")
}

case class BufferChildrenRef(val bufferIndex: Int) extends StateRef {
  require(bufferIndex >= 0, "the index of a BufferChildrenRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    BufferRef(bufferIndex)(state) flatMap { nodeIndex => state.children.getOrElse(nodeIndex, Seq()) }
  }

  @transient override val name: Symbol = Symbol(s"b${bufferIndex}c")
}

case class BufferChildRef(val bufferIndex: Int, val childIndex: Int) extends StateRef {
  require(childIndex >= 0, "the child index of a BufferChildRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    Seq(BufferChildrenRef(bufferIndex)(state).lift(childIndex)).flatten
  }

  @transient override val name: Symbol = Symbol(s"b${bufferIndex}c${childIndex}")
}

case class StackParentsRef(stackIndex: Int) extends StateRef {
  require(stackIndex >= 0, "the index of a StackParentsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    StackRef(stackIndex)(state) flatMap { nodeIndex =>
      state.getParents(nodeIndex)
    }
  }

  @transient override val name: Symbol = Symbol(s"s${stackIndex}p")
}

case class StackParentRef(val stackIndex: Int, val parentIndex: Int) extends StateRef {
  require(stackIndex >= 0, "the stack index of a StackParentRef must be a nonnegative integer")
  require(parentIndex >= 0, "the parent index of a StackParentRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    Seq(StackParentsRef(stackIndex)(state).lift(parentIndex)).flatten
  }

  @transient override val name: Symbol = Symbol(s"s${stackIndex}p${parentIndex}")
}

case class BufferParentsRef(bufferIndex: Int) extends StateRef {
  require(bufferIndex >= 0, "the index of a BufferParentsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    BufferRef(bufferIndex)(state) flatMap { nodeIndex =>
      state.getParents(nodeIndex)
    }
  }

  @transient override val name: Symbol = Symbol(s"b${bufferIndex}p")
}

case class BufferParentRef(val bufferIndex: Int, val parentIndex: Int) extends StateRef {
  require(bufferIndex >= 0, "the buffer index of a BufferParentRef must be a nonnegative integer")
  require(parentIndex >= 0, "the parent index of a BufferParentRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    Seq(BufferParentsRef(bufferIndex)(state).lift(parentIndex)).flatten
  }

  @transient override val name: Symbol = Symbol(s"b${bufferIndex}p${parentIndex}")
}

case class StackGretelsRef(val index: Int) extends StateRef {
  require(index >= 0, "the index of a StackGretelsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    StackRef(index)(state) flatMap { nodeIndex => state.getGretels(nodeIndex) }
  }

  @transient
  override val name: Symbol = Symbol("sg" + index)
}

case class StackLeftGretelsRef(val index: Int) extends StateRef {
  require(index >= 0, "the index of a StackLeftGretelsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    StackRef(index)(state) flatMap { nodeIndex =>
      state.getGretels(nodeIndex) filter { gretel =>
        gretel < nodeIndex
      }
    }
  }

  @transient
  override val name: Symbol = Symbol("sgl" + index)
}

case class StackRightGretelsRef(val index: Int) extends StateRef {
  require(index >= 0, "the index of a StackRightGretelsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    StackRef(index)(state) flatMap { nodeIndex =>
      state.getGretels(nodeIndex) filter { gretel =>
        gretel > nodeIndex
      }
    }
  }

  @transient
  override val name: Symbol = Symbol("sgr" + index)
}

case class BufferGretelsRef(val index: Int) extends StateRef {
  require(index >= 0, "the index of a BufferGretelsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    BufferRef(index)(state) flatMap { nodeIndex => state.getGretels(nodeIndex) }
  }

  @transient
  override val name: Symbol = Symbol("bg" + index)
}

case class BufferLeftGretelsRef(val index: Int) extends StateRef {
  require(index >= 0, "the index of a BufferLeftGretelsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    BufferRef(index)(state) flatMap { nodeIndex =>
      state.getGretels(nodeIndex) filter { gretel =>
        gretel < nodeIndex
      }
    }
  }

  @transient
  override val name: Symbol = Symbol("bgl" + index)
}

case class BufferRightGretelsRef(val index: Int) extends StateRef {
  require(index >= 0, "the index of a BufferLeftGretelsRef must be a nonnegative integer")

  override def apply(state: TransitionParserState): Seq[Int] = {
    BufferRef(index)(state) flatMap { nodeIndex =>
      state.getGretels(nodeIndex) filter { gretel =>
        gretel > nodeIndex
      }
    }
  }

  @transient
  override val name: Symbol = Symbol("bgr" + index)
}

/** A BreadcrumbRef is a StateRef (see above) whose apply operation returns the breadcrumb of
  * the `index`th element of the stack, if it exists.
  *
  * @param index the desired stack element, counting from 0 (i.e. 0 is the stack top)
  */
case class BreadcrumbRef(val index: Int) extends StateRef {
  require(index >= 0)

  override def apply(state: TransitionParserState): Seq[Int] = {
    if (index < state.stack.size && state.breadcrumb.getOrElse(state.stack(index), -1) >= 0) {
      Seq(state.breadcrumb(state.stack(index)))
    } else {
      Seq()
    }
  }

  @transient
  override val name: Symbol = Symbol("crumbRef" + index)
}

case object PreviousLinkCrumbRef extends StateRef {

  override def apply(state: TransitionParserState): Seq[Int] = {
    state.previousLink match {
      case Some((crumb, _)) => Seq(crumb)
      case None => Seq()
    }
  }

  @transient
  override val name: Symbol = Symbol("pc")
}

case object PreviousLinkCrumbGretelRef extends StateRef {

  override def apply(state: TransitionParserState): Seq[Int] = {
    PreviousLinkCrumbRef(state) flatMap { nodeIndex => state.getGretels(nodeIndex) }
  }

  @transient
  override val name: Symbol = Symbol("pcg")
}

case object PreviousLinkGretelRef extends StateRef {

  override def apply(state: TransitionParserState): Seq[Int] = {
    state.previousLink match {
      case Some((_, gretel)) => Seq(gretel)
      case None => Seq()
    }
  }

  @transient
  override val name: Symbol = Symbol("pg")
}

case object PreviousLinkGrandgretelRef extends StateRef {

  override def apply(state: TransitionParserState): Seq[Int] = {
    PreviousLinkGretelRef(state) flatMap { nodeIndex => state.getGretels(nodeIndex) }
  }

  @transient
  override val name: Symbol = Symbol("pgg")
}

