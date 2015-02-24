package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.polyparser._
import spray.json.DefaultJsonProtocol._
import spray.json._

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

  /** Boilerplate code to serialize a Transition to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM Transition, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object TransitionJsonFormat extends RootJsonFormat[StateTransition] {
    def write(transition: StateTransition): JsValue = transition match {
      case ArcEagerShift => JsString("Sh")
      case ArcEagerReduce => JsString("Re")
      case ArcHybridShift => JsString("HySh")
      case Fallback => JsString("Fb")
      case lt: ArcEagerLeftArc => {
        JsObject(leftArcFormat.write(lt).asJsObject.fields + ("type" -> JsString("Lt")))
      }
      case rt: ArcEagerRightArc => {
        JsObject(rightArcFormat.write(rt).asJsObject.fields + ("type" -> JsString("Rt")))
      }
      case hylt: ArcHybridLeftArc => {
        JsObject(hybridLeftArcFormat.write(hylt).asJsObject.fields + ("type" -> JsString("HyLt")))
      }
      case hyrt: ArcHybridRightArc => {
        JsObject(hybridRightArcFormat.write(hyrt).asJsObject.fields + ("type" -> JsString("HyRt")))
      }
      case larc: LabelLeftArc => {
        JsObject(leftLabelArcFormat.write(larc).asJsObject.fields +
          ("type" -> JsString("LtLbl")))
      }
      case larc: LabelRightArc => {
        JsObject(rightLabelArcFormat.write(larc).asJsObject.fields +
          ("type" -> JsString("RtLbl")))
      }
    }

    def read(value: JsValue): StateTransition = value match {
      case JsString(typeid) => typeid match {
        case "Sh" => ArcEagerShift
        case "Re" => ArcEagerReduce
        case "HySh" => ArcHybridShift
        case "Fb" => Fallback
        case x => deserializationError(s"Invalid identifier for Transition: $x")
      }
      case JsObject(values) => values("type") match {
        case JsString("Lt") => leftArcFormat.read(value)
        case JsString("Rt") => rightArcFormat.read(value)
        case JsString("HyLt") => hybridLeftArcFormat.read(value)
        case JsString("HyRt") => hybridRightArcFormat.read(value)
        case JsString("LtLbl") => leftLabelArcFormat.read(value)
        case JsString("RtLbl") => rightLabelArcFormat.read(value)
        case x => deserializationError(s"Invalid identifier for Transition: $x")
      }
      case _ => deserializationError("Unexpected JsValue type. Must be JsString or JsObject.")
    }
  }

  val leftArcFormat: RootJsonFormat[ArcEagerLeftArc] = jsonFormat1(ArcEagerLeftArc.apply)
  val rightArcFormat: RootJsonFormat[ArcEagerRightArc] = jsonFormat1(ArcEagerRightArc.apply)
  val hybridLeftArcFormat: RootJsonFormat[ArcHybridLeftArc] = jsonFormat1(ArcHybridLeftArc.apply)
  val hybridRightArcFormat: RootJsonFormat[ArcHybridRightArc] = jsonFormat1(ArcHybridRightArc.apply)
  val leftLabelArcFormat: RootJsonFormat[LabelLeftArc] = jsonFormat1(LabelLeftArc.apply)
  val rightLabelArcFormat: RootJsonFormat[LabelRightArc] = jsonFormat1(LabelRightArc.apply)
}

case object Fallback extends StateTransition {

  override def apply(state: Option[State]): Option[State] = None

  override val name: String = "Fb"
}
