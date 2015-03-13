package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.FeatureName

/** A StateCostFunctionTrainer trains a StateCostFunction from data. Training is
  * triggered during construction, after which the .costFunction field contains the trained
  * TransitionCostFunctionAndClassifier.
  *
  * @param trainingVectorSource a source of training vectors
  */
abstract class StateCostFunctionTrainer(
    transitionSystemFactory: TransitionSystemFactory, trainingVectorSource: FSMTrainingVectorSource
) {

  /** The trained cost function factory. */
  def costFunctionFactory: StateCostFunctionFactory

  protected val featureNames: List[FeatureName] =
    FSMTrainingVectorSource.collectFeatureNames(trainingVectorSource).toList

  protected val featureNameToIndex: Map[FeatureName, Int] = featureNames.zipWithIndex.toMap

  protected val transitions: IndexedSeq[StateTransition] =
    FSMTrainingVectorSource.collectTransitions(trainingVectorSource).toIndexedSeq
}

