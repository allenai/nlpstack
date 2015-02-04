package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.polyparser.{
  RequestedCpos,
  RequestedArc,
  ForbiddenArcLabel,
  ForbiddenEdge
}
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A TransitionConstraint returns true if a given transition is illegal to
  * apply in a given state.
  */
trait TransitionConstraint

object TransitionConstraint {
  implicit val forbiddenEdgeFormat =
    jsonFormat2(ForbiddenEdge.apply).pack("constraintType" -> "forbiddenEdge")

  implicit val forbiddenArcLabelFormat =
    jsonFormat3(ForbiddenArcLabel.apply).pack("constraintType" -> "forbiddenArcLabel")

  implicit val requestedArcFormat =
    jsonFormat(RequestedArc.apply, "token1", "token2", "arcLabel").pack(
      "constraintType" -> "requestedArc"
    )

  implicit val requestedCposFormat =
    jsonFormat2(RequestedCpos.apply).pack("constraintType" -> "requestedCpos")

  implicit object ParserConstraintFormat extends JsonFormat[TransitionConstraint] {
    override def read(jsValue: JsValue): TransitionConstraint = {
      jsValue.asJsObject.unpackWith[TransitionConstraint](
        forbiddenEdgeFormat,
        forbiddenArcLabelFormat,
        requestedArcFormat,
        requestedCposFormat
      )
    }

    override def write(constraint: TransitionConstraint): JsValue = constraint match {
      case forbiddenEdge: ForbiddenEdge => forbiddenEdge.toJson
      case forbiddenArcLabel: ForbiddenArcLabel => forbiddenArcLabel.toJson
      case requestedArc: RequestedArc => requestedArc.toJson
      case requestedCpos: RequestedCpos => requestedCpos.toJson
    }
  }
}

/** A ConstraintInterpretation tells you whether a transition is inapplicable in a given state.
  *
  * Specifically, it is a function that takes a (state, transition) pair, and returns true
  * if the transition is inapplicable.
  */
trait ConstraintInterpretation extends ((State, StateTransition) => Boolean)
