package org.allenai.nlpstack.parse.poly.fsm

import scala.annotation.tailrec

trait Search {
  /** Finds a low-cost walk through a finite-state machine.
    *
    * @param initialState the state of the FSM in which to begin the walk
    * @param constraints constraints that the returned walk must satisfy
    * @return a low-cost walk (if found); otherwise, None
    */
  def find(initialState: State, constraints: Set[TransitionConstraint]): Option[Walk]
}

class GreedySearch(costFunction: StateCostFunction) extends Search {
  private val baseParser = new NostalgicSearch(costFunction, qualifyingCostDelta = 10000)

  override def find(initialState: State,
    constraints: Set[TransitionConstraint]): Option[Walk] = {

    baseParser.completeWalk(Walk(initialState, Seq()), 0.0, constraints)
  }
}

/** Like the GreedyTransitionParser, except that it remembers promising transitions that were not
  * taken from the greedy (one-best) walk and returns those to the user.
  *
  * @param costFunction the cost function that the parser should use to evaluate the transitions
  * @param qualifyingCostDelta the parser will retain walks for suboptimal transitions whose costs
  * are no worse than qualifyingCostDelta more than the optimal transition cost
  */
class NostalgicSearch(val costFunction: StateCostFunction, qualifyingCostDelta: Double) {

  /** Determines a set of "promising" walks through a finite-state machine.
    *
    * The return value is a set of "good" walks, each associated with a cost value (higher =
    * worse quality).
    *
    * @param walk the walk to continue
    * @param constraints any constraints to impose on the returned walks
    * @return a set of good walks that satisfy the input constaints, associated with their costs
    */
  def getPromisingWalks(walk: Walk, costSoFar: Double,
    constraints: Set[TransitionConstraint] = Set()): (Seq[ScoredWalk], Boolean) = {

    parseRecursive(walk.finalState, costSoFar, walk.steps, Seq(), constraints, false)
  }

  def completeWalk(walk: Walk, costSoFar: Double,
    constraints: Set[TransitionConstraint]): Option[Walk] = {

    val (promisingWalks, _) = getPromisingWalks(walk, costSoFar, constraints)
    promisingWalks.headOption match {
      case Some(scoredWalk) =>
        Some(Walk(walk.initialState, walk.steps ++ scoredWalk.walk.steps))
      case _ => None
    }
  }

  @tailrec protected final def parseRecursive(initState: Option[State],
    costSoFar: Double, stepsSoFar: Seq[WalkStep], mementosSoFar: Seq[ScoredWalk],
    constraints: Set[TransitionConstraint],
    constraintEncountered: Boolean): (Seq[ScoredWalk], Boolean) = {

    initState match {
      case None => (mementosSoFar, constraintEncountered)
      case Some(initialState) if initialState.isFinal =>
        (ScoredWalk(Walk(initialState, stepsSoFar), costSoFar) +: mementosSoFar,
          constraintEncountered)
      case Some(initialState) =>
        val transitionCosts: List[(StateTransition, Double)] =
          costFunction(initialState).toList sortBy { x => x._2 }

        // Filter out transitions that are disallowed by transition and parser constraints.
        val applicableTransitionCosts = transitionCosts filter {
          case (transition, _) => transition(initState) != None
        }
        val globallyBestTransition: Option[StateTransition] =
          if (applicableTransitionCosts.nonEmpty) {
            Some(applicableTransitionCosts.head._1)
          } else {
            None
          }

        val filteredTransitionCosts: List[(StateTransition, Double)] =
          applicableTransitionCosts filter {
            case (transition, _) =>
              !(constraints map { x =>
                val constraintInterpretation =
                  costFunction.transitionSystem.interpretConstraint(x)
                constraintInterpretation(initialState, transition)
              }).contains(true)
          }
        val promisingTransitions: List[(StateTransition, Double)] =
          collectPromisingTransitions(filteredTransitionCosts)
        promisingTransitions.headOption match {
          case Some((bestTransition, bestCost)) =>
            val runnerupWalks: Seq[ScoredWalk] = promisingTransitions.tail map {
              case (transition, cost) => ScoredWalk(Walk(initialState,
                stepsSoFar :+ WalkStep(initialState, transition)),
                costSoFar + cost)
            }
            this.parseRecursive(bestTransition(initState), costSoFar + bestCost,
              stepsSoFar :+ WalkStep(initialState, bestTransition),
              mementosSoFar ++ runnerupWalks, constraints,
              constraintEncountered || (Some(bestTransition) != globallyBestTransition))
          case None => (mementosSoFar, true)
        }
    }
  }

  private def collectPromisingTransitions(
    transitionCosts: List[(StateTransition, Double)]): List[(StateTransition, Double)] = {

    transitionCosts.headOption match {
      case Some((_, minCost)) =>
        transitionCosts filter {
          case (_, cost) =>
            cost - minCost < qualifyingCostDelta
        }
      case None => List()
    }
  }
}
