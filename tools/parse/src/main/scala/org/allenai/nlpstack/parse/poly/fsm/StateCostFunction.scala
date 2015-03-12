package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, FeatureName }
import spray.json.DefaultJsonProtocol._
import spray.json._
import org.allenai.common.json._

/** A StateCostFunction assigns a (real-valued) cost to the Transitions that can potentially
  * be applied to a State. Generally speaking: the lower the cost, the better
  * the transition.
  *
  * Typically, instances of StateCostFunction will compute this cost using a feature
  * representation of the State. But this is not always the case -- see the
  * GuidedCostFunction in [[org.allenai.nlpstack.parse.poly.polyparser.ArcEagerGuidedCostFunction]]
  * for a cost function that uses a gold parse tree as the basis for its cost function.
  */
abstract class StateCostFunction extends (State => Map[StateTransition, Double]) {

  def transitionSystem: TransitionSystem

  def lowestCostTransition(state: State): Option[StateTransition] = {
    val transitionCosts = this.apply(state)
    if (transitionCosts.isEmpty) {
      None
    } else {
      Some((transitionCosts minBy (_._2))._1)
    }
  }
}

object StateCostFunction {
  implicit object StateJsonFormat extends RootJsonFormat[StateCostFunction] {
    implicit val classifierBasedCostFunctionFormat =
      jsonFormat5(ClassifierBasedCostFunction.apply).pack("type" -> "ClassifierBasedCostFunction")

    def write(costFunction: StateCostFunction): JsValue = costFunction match {
      case cbCostFunction: ClassifierBasedCostFunction => cbCostFunction.toJson
      case x => deserializationError(s"Cannot serialize this cost function type: $x")
    }

    def read(value: JsValue): StateCostFunction = value.asJsObject.unpackWith(
      classifierBasedCostFunctionFormat
    )
  }
}

case class ClassifierBasedCostFunction(
  transitionSystem: TransitionSystem, transitions: Seq[StateTransition],
  taskClassifierList: List[(ClassificationTask, TransitionClassifier)],
  baseCostFunction: Option[StateCostFunction] = None
)
    extends StateCostFunction {

  @transient
  lazy val taskClassifiers = taskClassifierList.toMap

  override def apply(state: State): Map[StateTransition, Double] = {
    transitionCosts(state, 0.0)
  }

  /** Returns a distribution over all possible transitions, according to the classifier associated
    * with the given task.
    *
    * The return value will be a map from transitions to their probabilities. The `minProb`
    * argument tells this function not to bother including transitions whose probabilities are
    * less than `minProb`.
    *
    * If there were no training examples for the task, then the uniform distribution is returned.
    *
    * @param state the parser state
    * @param minProb only include transitions in the returned map if their
    * probability is greater than this bound
    * @return a map from transitions to their probabilities
    */
  private def transitionDistribution(
    state: State,
    minProb: Double
  ): Map[StateTransition, Double] = {

    transitionSystem.taskIdentifier(state) match {
      case Some(task) =>
        val featureVector: FeatureVector = transitionSystem.computeFeature(state)
        val topLevelDistribution: Map[StateTransition, Double] = {
          if (!taskClassifiers.contains(task)) {
            transitions.zip(transitions.map { _ =>
              1.0 / transitions.size
            }).toMap
          } else {
            taskClassifiers(task).getDistribution(featureVector) filter {
              case (transition, prob) => prob >= minProb
            }
          }
        }
        val result = if (topLevelDistribution.contains(Fallback)) {
          require(baseCostFunction != None)
          val baseCosts: Map[StateTransition, Double] = (baseCostFunction.get)(state)
          val baseDistribution = baseCosts mapValues (x => Math.exp(-x))
          val fallbackProb: Double = topLevelDistribution(Fallback)
          val topLevelDistributionWithoutFallback = topLevelDistribution - Fallback
          (for {
            key <- baseDistribution.keys ++ topLevelDistributionWithoutFallback.keys
          } yield (key, topLevelDistributionWithoutFallback.getOrElse(key, 0.0) +
            fallbackProb * baseDistribution.getOrElse(key, 0.0))).toMap
        } else {
          topLevelDistribution
        }
        result
      case None => Map()
    }
  }

  /** Returns negative log of [[transitionDistribution()]].
    *
    * This is the negative log of a distribution over all possible transitions,
    * according to the classifier associated with the given task.
    *
    * The return value will be a map from transitions to their probabilities. The `minProb`
    * argument tells this function not to bother including transitions whose probabilities are
    * less than `minProb`.
    *
    * If there were no training examples for the task, then the uniform distribution is used.
    *
    * @param state the parser state
    * @param minProb only include transitions in the returned map if their
    * probability is greater than this bound
    * @return a map from transitions to negative log of their neprobabilities
    */
  private def transitionCosts(
    state: State,
    minProb: Double
  ): Map[StateTransition, Double] = {

    transitionDistribution(state, minProb) mapValues (-Math.log(_))
  }

}
