package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.fsm._

/** A GoldParseSource reduces parse trees to states of a finite-state machine.
  *
  * @param goldParses the source for the parse trees
  * @param transitionSystem the transition system to use (for generating states)
  */
case class GoldParseSource(goldParses: PolytreeParseSource, transitionSystem: TransitionSystem)
    extends StateSource {

  def getStateIterator: Iterator[State] = {
    for {
      goldParse <- goldParses.parseIterator
      state <- convertToTransitionParserStates(goldParse)
    } yield state
  }

  private def convertToTransitionParserStates(
    goldParse: PolytreeParse): Seq[State] = {

    transitionSystem.guidedCostFunction(goldParse) match {
      case Some(costFunc) =>
        val search = new GreedySearch(costFunc)
        val initialState = transitionSystem.initialState(goldParse.sentence)

        val bestWalk: Option[Walk] = initialState flatMap { initState =>
          search.find(initState, Set())
        }
        bestWalk match {
          case Some(walk) => walk.states
          case None => Seq()
        }
      case None => Seq()
    }
  }
}

/** A GoldParseTrainingVectorSource reduces a gold parse tree to a set of feature vectors for
  * classifier training.
  *
  * Essentially, we derive the 2*n parser states that lead to the gold parse. Each of these states
  * becomes a feature vector (using the apply method of the provided TransitionParserFeature),
  * labeled with the transition executed from that state in the gold parse.
  *
  * One of the constructor arguments is a TaskIdentifer. This will dispatch the feature vectors
  * to train different classifiers. For instance, if taskIdentifier(state) !=
  * taskIdentifier(state2), then their respective feature vectors (i.e. feature(state) and
  * feature(state2)) will be used to train different classifiers.
  *
  * @param goldParses the data source for the parse trees
  * @param taskIdentifier identifies the ClassificationTask associated with each feature vector
  * @param transitionSystem the transition system to use (for generating states)
  * @param baseCostFunction a trained cost function to adapt (optional)
  */
case class GoldParseTrainingVectorSource(goldParses: PolytreeParseSource,
  taskIdentifier: TaskIdentifier,
  transitionSystem: TransitionSystem,
  baseCostFunction: Option[StateCostFunction] = None)
    extends FSMTrainingVectorSource(taskIdentifier, transitionSystem, baseCostFunction) {

  def getVectorIterator: Iterator[FSMTrainingVector] = {
    for {
      goldParse <- goldParses.parseIterator
      vector <- generateVectors(goldParse)
    } yield vector
  }
}
