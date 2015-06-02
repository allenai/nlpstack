package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.polyparser._
import org.allenai.nlpstack.parse.poly.polytagger.AssignTag
import reming.DefaultJsonProtocol._

abstract class StateTransition extends (Option[State] => Option[State]) {
  val name: String
  override def toString: String = name
}

object StateTransition {
  def applyTransitionSequence(
    initialState: State,
    transitions: Seq[StateTransition]
  ): Option[State] = {

    transitions.foldLeft(Option(initialState)) { (state, transition) => transition(state) }
  }

  def applicable(transition: StateTransition, state: Option[State]): Boolean = {
    transition(state) != None
  }

  private implicit val arcEagerShiftFormat = jsonFormat0(() => ArcEagerShift)
  private implicit val arcEagerReduceFormat = jsonFormat0(() => ArcEagerReduce)
  private implicit val arcHybridShiftFormat = jsonFormat0(() => ArcHybridShift)
  private implicit val fallbackFormat = jsonFormat0(() => Fallback)
  private implicit val leftArcFormat = jsonFormat1(ArcEagerLeftArc.apply)
  private implicit val rightArcFormat = jsonFormat1(ArcEagerRightArc.apply)
  private implicit val hybridLeftArcFormat = jsonFormat1(ArcHybridLeftArc.apply)
  private implicit val hybridRightArcFormat = jsonFormat1(ArcHybridRightArc.apply)
  private implicit val leftLabelArcFormat = jsonFormat1(LabelLeftArc.apply)
  private implicit val rightLabelArcFormat = jsonFormat1(LabelRightArc.apply)
  private implicit val tagTokenFormat = jsonFormat1(AssignTag.apply)

  implicit val stateTransitionJsonFormat = parentFormat[StateTransition](
    childFormat[ArcEagerShift.type, StateTransition]("Sh"),
    childFormat[ArcEagerReduce.type, StateTransition]("Re"),
    childFormat[ArcHybridShift.type, StateTransition]("HySh"),
    childFormat[Fallback.type, StateTransition]("Fb"),
    childFormat[ArcEagerLeftArc, StateTransition]("Lt"),
    childFormat[ArcEagerRightArc, StateTransition]("Rt"),
    childFormat[ArcHybridLeftArc, StateTransition]("HyLt"),
    childFormat[ArcHybridRightArc, StateTransition]("HyRt"),
    childFormat[LabelLeftArc, StateTransition]("LtLbl"),
    childFormat[LabelRightArc, StateTransition]("RtLbl"),
    childFormat[AssignTag, StateTransition]("Tag")
  )
}

case object Fallback extends StateTransition {

  override def apply(state: Option[State]): Option[State] = None

  override val name: String = "Fb"
}
