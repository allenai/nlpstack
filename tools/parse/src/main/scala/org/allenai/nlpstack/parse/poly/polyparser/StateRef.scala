package org.allenai.nlpstack.parse.poly.polyparser

import spray.json._
import spray.json.DefaultJsonProtocol._

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

  /** Boilerplate code to serialize a StateRef to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM StateRef, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object StateRefJsonFormat extends RootJsonFormat[StateRef] {
    def write(stateRef: StateRef): JsValue = stateRef match {
      case LastRef => JsString("LastRef")
      case FirstRef => JsString("FirstRef")
      case PreviousLinkCrumbRef => JsString("PreviousLinkCrumbRef")
      case PreviousLinkGretelRef => JsString("PreviousLinkGretelRef")
      case PreviousLinkCrumbGretelRef => JsString("PreviousLinkCrumbGretelRef")
      case PreviousLinkGrandgretelRef => JsString("PreviousLinkGrandgretelRef")
      case stackRef: StackRef => {
        JsObject(stackRefFormat.write(stackRef).asJsObject.fields +
          ("type" -> JsString("StackRef")))
      }
      case bufferRef: BufferRef => {
        JsObject(bufferRefFormat.write(bufferRef).asJsObject.fields +
          ("type" -> JsString("BufferRef")))
      }
      case breadcrumbRef: BreadcrumbRef => {
        JsObject(breadcrumbRefFormat.write(breadcrumbRef).asJsObject.fields +
          ("type" -> JsString("BreadcrumbRef")))
      }
      case stackChildrenRef: StackChildrenRef => {
        JsObject(stackChildrenRefFormat.write(stackChildrenRef).asJsObject.fields +
          ("type" -> JsString("StackChildrenRef")))
      }
      case stackChildRef: StackChildRef => {
        JsObject(stackChildRefFormat.write(stackChildRef).asJsObject.fields +
          ("type" -> JsString("StackChildRef")))
      }
      case bufferChildrenRef: BufferChildrenRef => {
        JsObject(bufferChildrenRefFormat.write(bufferChildrenRef).asJsObject.fields +
          ("type" -> JsString("BufferChildrenRef")))
      }
      case bufferChildRef: BufferChildRef => {
        JsObject(bufferChildRefFormat.write(bufferChildRef).asJsObject.fields +
          ("type" -> JsString("BufferChildRef")))
      }
      case stackParentsRef: StackParentsRef => {
        JsObject(stackParentsRefFormat.write(stackParentsRef).asJsObject.fields +
          ("type" -> JsString("StackParentsRef")))
      }
      case stackParentRef: StackParentRef => {
        JsObject(stackParentRefFormat.write(stackParentRef).asJsObject.fields +
          ("type" -> JsString("StackParentRef")))
      }
      case bufferParentsRef: BufferParentsRef => {
        JsObject(bufferParentsRefFormat.write(bufferParentsRef).asJsObject.fields +
          ("type" -> JsString("BufferParentsRef")))
      }
      case bufferParentRef: BufferParentRef => {
        JsObject(bufferParentRefFormat.write(bufferParentRef).asJsObject.fields +
          ("type" -> JsString("BufferParentRef")))
      }
      case stackGretelsRef: StackGretelsRef => {
        JsObject(stackGretelsRefFormat.write(stackGretelsRef).asJsObject.fields +
          ("type" -> JsString("StackGretelsRef")))
      }
      case stackLeftGretelsRef: StackLeftGretelsRef => {
        JsObject(stackLeftGretelsRefFormat.write(stackLeftGretelsRef).asJsObject.fields +
          ("type" -> JsString("StackLeftGretelsRef")))
      }
      case stackRightGretelsRef: StackRightGretelsRef => {
        JsObject(stackRightGretelsRefFormat.write(stackRightGretelsRef).asJsObject.fields +
          ("type" -> JsString("StackRightGretelsRef")))
      }
      case bufferGretelsRef: BufferGretelsRef => {
        JsObject(bufferGretelsRefFormat.write(bufferGretelsRef).asJsObject.fields +
          ("type" -> JsString("BufferGretelsRef")))
      }
      case bufferLeftGretelsRef: BufferLeftGretelsRef => {
        JsObject(bufferLeftGretelsRefFormat.write(bufferLeftGretelsRef).asJsObject.fields +
          ("type" -> JsString("BufferLeftGretelsRef")))
      }
      case bufferRightGretelsRef: BufferRightGretelsRef => {
        JsObject(bufferRightGretelsRefFormat.write(bufferRightGretelsRef).asJsObject.fields +
          ("type" -> JsString("BufferRightGretelsRef")))
      }
    }

    def read(value: JsValue): StateRef = value match {
      case JsString(typeid) => typeid match {
        case "LastRef" => LastRef
        case "FirstRef" => FirstRef
        case "PreviousLinkCrumbRef" => PreviousLinkCrumbRef
        case "PreviousLinkGretelRef" => PreviousLinkGretelRef
        case "PreviousLinkCrumbGretelRef" => PreviousLinkCrumbGretelRef
        case "PreviousLinkGrandgretelRef" => PreviousLinkGrandgretelRef
      }
      case JsObject(values) => values("type") match {
        case JsString("StackRef") => stackRefFormat.read(value)
        case JsString("BufferRef") => bufferRefFormat.read(value)
        case JsString("BreadcrumbRef") => breadcrumbRefFormat.read(value)
        case JsString("StackChildrenRef") => stackChildrenRefFormat.read(value)
        case JsString("StackChildRef") => stackChildRefFormat.read(value)
        case JsString("BufferChildrenRef") => bufferChildrenRefFormat.read(value)
        case JsString("BufferChildRef") => bufferChildRefFormat.read(value)
        case JsString("StackParentsRef") => stackParentsRefFormat.read(value)
        case JsString("StackParentRef") => stackParentRefFormat.read(value)
        case JsString("BufferParentsRef") => bufferParentsRefFormat.read(value)
        case JsString("BufferParentRef") => bufferParentRefFormat.read(value)

        case JsString("StackGretelsRef") => stackGretelsRefFormat.read(value)
        case JsString("StackLeftGretelsRef") => stackLeftGretelsRefFormat.read(value)
        case JsString("StackRightGretelsRef") => stackRightGretelsRefFormat.read(value)
        case JsString("BufferGretelsRef") => bufferGretelsRefFormat.read(value)
        case JsString("BufferLeftGretelsRef") => bufferLeftGretelsRefFormat.read(value)
        case JsString("BufferRightGretelsRef") => bufferRightGretelsRefFormat.read(value)
        case x => deserializationError(s"Invalid identifier for StateRef: $x")
      }
      case _ => deserializationError("Unexpected JsValue type. Must be JsString or JsObject.")
    }
  }

  val stackRefFormat: RootJsonFormat[StackRef] = jsonFormat1(StackRef.apply)
  val bufferRefFormat: RootJsonFormat[BufferRef] = jsonFormat1(BufferRef.apply)
  val breadcrumbRefFormat: RootJsonFormat[BreadcrumbRef] = jsonFormat1(BreadcrumbRef.apply)
  val stackChildrenRefFormat: RootJsonFormat[StackChildrenRef] =
    jsonFormat1(StackChildrenRef.apply)
  val stackChildRefFormat: RootJsonFormat[StackChildRef] =
    jsonFormat2(StackChildRef.apply)
  val bufferChildrenRefFormat: RootJsonFormat[BufferChildrenRef] =
    jsonFormat1(BufferChildrenRef.apply)
  val bufferChildRefFormat: RootJsonFormat[BufferChildRef] =
    jsonFormat2(BufferChildRef.apply)
  val stackParentsRefFormat: RootJsonFormat[StackParentsRef] =
    jsonFormat1(StackParentsRef.apply)
  val stackParentRefFormat: RootJsonFormat[StackParentRef] =
    jsonFormat2(StackParentRef.apply)
  val bufferParentsRefFormat: RootJsonFormat[BufferParentsRef] =
    jsonFormat1(BufferParentsRef.apply)
  val bufferParentRefFormat: RootJsonFormat[BufferParentRef] =
    jsonFormat2(BufferParentRef.apply)
  val stackGretelsRefFormat: RootJsonFormat[StackGretelsRef] = jsonFormat1(StackGretelsRef.apply)
  val stackLeftGretelsRefFormat: RootJsonFormat[StackLeftGretelsRef] =
    jsonFormat1(StackLeftGretelsRef.apply)
  val stackRightGretelsRefFormat: RootJsonFormat[StackRightGretelsRef] =
    jsonFormat1(StackRightGretelsRef.apply)
  val bufferGretelsRefFormat: RootJsonFormat[BufferGretelsRef] =
    jsonFormat1(BufferGretelsRef.apply)
  val bufferLeftGretelsRefFormat: RootJsonFormat[BufferLeftGretelsRef] =
    jsonFormat1(BufferLeftGretelsRef.apply)
  val bufferRightGretelsRefFormat: RootJsonFormat[BufferRightGretelsRef] =
    jsonFormat1(BufferRightGretelsRef.apply)
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

