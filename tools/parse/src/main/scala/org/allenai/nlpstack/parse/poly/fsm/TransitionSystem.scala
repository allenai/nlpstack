package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.ml.FeatureVector
import org.allenai.nlpstack.parse.poly.polyparser.{ ArcHybridTransitionSystemFactory, ArcEagerTransitionSystemFactory, ArcHybridTransitionSystem, ArcEagerTransitionSystem }
import spray.json.DefaultJsonProtocol._
import spray.json._

trait TransitionSystem {
  val taskIdentifier: TaskIdentifier
  def initialState(constraints: Seq[TransitionConstraint]): Option[State]
  def guidedCostFunction(goldObj: Sculpture): Option[StateCostFunction]
  def computeFeature(state: State): FeatureVector
  def toSculpture(state: State): Option[Sculpture]
  def interpretConstraint(constraint: TransitionConstraint): ((State, StateTransition) => Boolean)
}

object TransitionSystem {
  def trivialConstraint(state: State, transition: StateTransition): Boolean = false
}

/** A TransitionSystemFactory is a factory that constructs marbleblock-specific transition
  * systems. For instance, in parsing, this would create a transition system for each input
  * sentence that you want to parse.
  */
trait TransitionSystemFactory {
  def buildTransitionSystem(
    marbleBlock: MarbleBlock,
    constraints: Set[TransitionConstraint]
  ): TransitionSystem
}

object TransitionSystemFactory {

  implicit object TransitionSystemFactoryJsonFormat
      extends RootJsonFormat[TransitionSystemFactory] {

    implicit val arcEagerFormat =
      jsonFormat1(ArcEagerTransitionSystemFactory.apply).pack(
        "type" -> "ArcEagerTransitionSystemFactory"
      )
    implicit val arcHybridFormat =
      jsonFormat1(ArcHybridTransitionSystemFactory.apply).pack(
        "type" -> "ArcHybridTransitionSystemFactory"
      )

    def write(transitionSystemFactory: TransitionSystemFactory): JsValue =
      transitionSystemFactory match {
        case aeSys: ArcEagerTransitionSystemFactory => aeSys.toJson
        case ahSys: ArcHybridTransitionSystemFactory => ahSys.toJson
        case x => deserializationError(s"Cannot serialize this state type: $x")
      }

    def read(value: JsValue): TransitionSystemFactory = value match {
      case JsString(typeid) => typeid match {
        case x => deserializationError(s"Invalid identifier for TaskIdentifier: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(arcEagerFormat, arcHybridFormat)
      case _ => deserializationError("Unexpected JsValue type.")
    }
  }
}
