package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.FeatureVector
import org.allenai.nlpstack.parse.poly.polyparser.{
  ArcEagerTransitionSystemFactory,
  ArcHybridTransitionSystemFactory
}
import org.allenai.nlpstack.parse.poly.polytagger.PostaggerTransitionSystemFactory

import reming.DefaultJsonProtocol._

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
  private implicit val arcHybridFormat = jsonFormat1(ArcHybridTransitionSystemFactory.apply)
  private implicit val arcEagerFormat = jsonFormat1(ArcEagerTransitionSystemFactory.apply)
  private implicit val postaggerFormat = jsonFormat1(PostaggerTransitionSystemFactory.apply)
  implicit val transitionSystemFactoryJsonFormat = parentFormat[TransitionSystemFactory](
    childFormat[ArcHybridTransitionSystemFactory, TransitionSystemFactory],
    childFormat[ArcEagerTransitionSystemFactory, TransitionSystemFactory],
    childFormat[PostaggerTransitionSystemFactory, TransitionSystemFactory]
  )
}
