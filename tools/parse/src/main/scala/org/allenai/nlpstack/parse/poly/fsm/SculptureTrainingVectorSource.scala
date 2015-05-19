package org.allenai.nlpstack.parse.poly.fsm

/** A SculptureTrainingVectorSource reduces a sculpture to a set of feature vectors for
  * classifier training.
  *
  * Essentially, we derive the transition states that lead to the gold sculpture.
  * Each of these states becomes a feature vector,
  * labeled with the transition executed from that state in the gold sculpture.
  *
  * One of the constructor arguments is a TaskIdentifer. This will dispatch the feature vectors
  * to train different classifiers. For instance, if taskIdentifier(state) !=
  * taskIdentifier(state2), then their respective feature vectors (i.e. feature(state) and
  * feature(state2)) will be used to train different classifiers.
  *
  * @param sculptureSource the data source for the training sculptures
  * @param transitionSystemFactory the transition system factory to use (for generating states)
  * @param baseCostFunctionFactory a trained cost function factory to adapt (optional)
  */
case class SculptureTrainingVectorSource(
  sculptureSource: SculptureSource,
  transitionSystemFactory: TransitionSystemFactory,
  baseCostFunctionFactory: Option[StateCostFunctionFactory] = None
)
    extends FSMTrainingVectorSource(transitionSystemFactory, baseCostFunctionFactory) {

  def getVectorIterator: Iterator[FSMTrainingVector] = {
    for {
      taggedSentence <- sculptureSource.sculptureIterator
      vector <- generateVectors(taggedSentence)
    } yield {
      println(vector)
      vector
    }
  }
}
