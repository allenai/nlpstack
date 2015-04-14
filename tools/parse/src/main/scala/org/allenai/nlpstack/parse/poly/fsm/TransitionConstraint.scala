package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.polyparser.{
  RequestedCpos,
  RequestedArc,
  ForbiddenArcLabel,
  ForbiddenEdge
}
import reming.DefaultJsonProtocol._

/** A TransitionConstraint returns true if a given transition is illegal to
  * apply in a given state.
  */
trait TransitionConstraint

object TransitionConstraint {
  private implicit val forbiddenEdgeFormat = jsonFormat2(ForbiddenEdge.apply)
  private implicit val forbiddenArcLabelFormat = jsonFormat3(ForbiddenArcLabel.apply)
  private implicit val requestedArcFormat = jsonFormat3(RequestedArc.apply)
  private implicit val requestedCposFormat = jsonFormat2(RequestedCpos.apply)
  implicit val parserConstraintFormat = parentFormat[TransitionConstraint](
    childFormat[ForbiddenEdge, TransitionConstraint],
    childFormat[ForbiddenArcLabel, TransitionConstraint],
    childFormat[RequestedArc, TransitionConstraint],
    childFormat[RequestedCpos, TransitionConstraint]
  )
}

/** A ConstraintInterpretation tells you whether a transition is inapplicable in a given state.
  *
  * Specifically, it is a function that takes a (state, transition) pair, and returns true
  * if the transition is inapplicable.
  */
trait ConstraintInterpretation extends ((State, StateTransition) => Boolean)
