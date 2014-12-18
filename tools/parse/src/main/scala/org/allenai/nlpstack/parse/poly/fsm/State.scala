package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.polyparser.TransitionParserState
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A state of a finite-state machine. */
trait State {
  val isFinal: Boolean
  def asSculpture: Option[Sculpture]
}

object State {
  implicit object StateJsonFormat extends RootJsonFormat[State] {
    implicit val transitionParserStateFormat =
      jsonFormat6(TransitionParserState.apply).pack("type" -> "TransitionParserState")

    def write(state: State): JsValue = state match {
      case tpState: TransitionParserState => tpState.toJson
      case x => deserializationError(s"Cannot serialize this state type: $x")
    }

    def read(value: JsValue): State = value.asJsObject.unpackWith(
      transitionParserStateFormat)
  }
}

/** A StateCost maps a state to a cost. */
trait StateCost extends (Option[State] => Double)

trait StateSource {
  /** Generates an iterator over State objects.
    *
    * @return a use-once iterator over State objects
    */
  def getStateIterator: Iterator[State]
}

/** A StateSource that keeps all its states in memory. */
case class InMemoryStateSource(states: Iterable[State]) extends StateSource {
  override def getStateIterator: Iterator[State] = states.iterator
}
