package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.polyparser.TransitionParserState

import reming.DefaultJsonProtocol._

/** A state of a finite-state machine. */
trait State {
  val isFinal: Boolean
  def asSculpture: Option[Sculpture]
}

object State {
  private implicit val transitionParserStateFormat = jsonFormat8(TransitionParserState.apply)
  implicit val stateJsonFormat = parentFormat[State](childFormat[TransitionParserState, State])
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
