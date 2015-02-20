package org.allenai.nlpstack.parse.poly.fsm

import scala.collection.mutable

/** Finds the best n greedy paths through a finite-state machine.
  *
  * @param costFunction the cost function to use to evaluate transitions from a given state
  */
class NbestSearch(
    costFunction: StateCostFunction,
    timeout: Int = NbestSearch.defaultTimeout
) {

  // Right now, we use a rather generous "qualifying cost delta" of 10000.0, to make sure that
  // most reasonable alternatives are remembered by the nostalgic parser.
  val baseParser: NostalgicSearch = new NostalgicSearch(costFunction, 10000.0)

  /** Finds the best n greedy paths through a finite-state machine.
    *
    * @param initialState the initial state in the finite-state machine
    * @param maxDesiredWalks the number of walks desired (i.e. n)
    * @param constraints a set of constraints that must be satisfied by returned paths
    * @return an n-best list containing n greedy paths through the FSM
    */
  def find(initialState: State, maxDesiredWalks: Int,
    constraints: Set[TransitionConstraint] = Set()): NbestList = {

    val queue = mutable.PriorityQueue[ScoredWalk]()(
      Ordering.by({ walk: ScoredWalk => -walk.score })
    )
    var results: Seq[ScoredWalk] = Seq()
    var iterNumber: Int = 0
    queue.enqueue(ScoredWalk(Walk(initialState, Seq()), 0.0))
    while (queue.nonEmpty && results.size < maxDesiredWalks && iterNumber < timeout) {
      iterNumber += 1
      val scoredWalk: ScoredWalk = queue.dequeue()
      if (scoredWalk.walk.isGoal) {
        results = scoredWalk +: results
      } else {
        val (mementos, _) =
          baseParser.getPromisingWalks(scoredWalk.walk, scoredWalk.score, constraints)
        mementos.headOption match {
          case Some(memento) =>
            if (memento.walk.isGoal) {
              results = memento +: results
              queue ++= mementos.tail
            } else {
              queue ++= mementos
            }
          case _ =>
        }
      }
    }
    val allWalks: Seq[ScoredWalk] = results
    NbestList(
      (allWalks map { scoredWalk =>
      scoredWalk.walk.finalState flatMap { state =>
        state.asSculpture
      } map { sculpture =>
        (sculpture, scoredWalk.score)
      }
    }).flatten
    )
  }
}

object NbestSearch {
  val defaultTimeout = 1000
}
