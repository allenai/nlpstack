package org.allenai.nlpstack.parse.poly.fsm

import reming.DefaultJsonProtocol._

/** A WalkStep is a single step in an FSM walk.
  *
  * @param state the current state
  * @param transition the transition to take
  * param transitionCosts the costs of the possible transitions in the current state
  */
case class WalkStep(state: State, transition: StateTransition)

object WalkStep {
  implicit val jsFormat = jsonFormat2(WalkStep.apply)
}

/** A Walk is a walk through a finite-state machine.
  *
  * @param initialState the state in which we begin
  * @param steps the sequence of steps we take from the initial state
  */
case class Walk(initialState: State, steps: Seq[WalkStep]) {

  /** The sequence of transitions taken during this walk (in order). */
  lazy val transitions = steps map { case WalkStep(_, transition) => transition }

  /** The sequence of states encountered during this walk (in order). */
  lazy val states: Seq[State] = {
    finalState match {
      case Some(reachableState) =>
        val walkStates: Seq[State] = steps map { step => step.state }
        walkStates :+ reachableState
      case None => Seq()
    }
  }

  /** Returns the parser state that results from executing the steps of this parse step, starting
    * from the initial state.
    */
  lazy val finalState: Option[State] = {
    if (steps.isEmpty) {
      Some(initialState)
    } else {
      (transitions.last)(Some(steps.last.state))
    }
  }

  /** Returns whether this walk ends up in a goal state. */
  lazy val isGoal: Boolean = {
    finalState match {
      case Some(state) => state.isFinal
      case _ => false
    }
  }

  override def toString: String = {
    "[" + (steps map { _.transition }).mkString(" ") + "]"
  }
}

object Walk {
  implicit val jsFormat = jsonFormat2(Walk.apply)
}

/** A ScoredWalk attaches a score to a Walk.
  *
  * @param walk the unscored Walk
  * @param score the floating-point score
  */
case class ScoredWalk(walk: Walk, score: Double)

object ScoredWalk {
  implicit val jsFormat = jsonFormat2(ScoredWalk.apply)
}
