package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.decisiontree.{
  FeatureVector => DTFeatureVector,
  FeatureVectors => DTFeatureVectors,
  _
}
import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }

import scala.collection.immutable.HashSet

/** An EmbeddedClassifier wraps a
  * [[org.allenai.nlpstack.parse.poly.decisiontree.ProbabilisticClassifier]] implementation
  * to provide a classifier interface that maps Transitions to probabilities.
  *
  * @param classifier the underlying classifier
  * @param transitions the possible outcomes of the underlying classifier
  * @param featureNameMap a list of the feature indices followed by their names
  * @param numFeatures the number of features in the underlying classifier
  */
case class EmbeddedClassifier(
    classifier: ProbabilisticClassifier,
    transitions: IndexedSeq[StateTransition],
    featureNameMap: Seq[(Int, FeatureName)],
    numFeatures: Int
) extends TransitionClassifier {

  @transient
  private val featureNameToIndex: Map[FeatureName, Int] =
    (featureNameMap map {
      case (featIndex, feat) =>
        (feat, featIndex)
    }).toMap

  override def classify(featureVector: FeatureVector): StateTransition = {
    transitions(classifier.classify(createDTFeatureVector(featureVector)))
  }

  override def getDistribution(featureVector: FeatureVector): Map[StateTransition, Double] = {
    val dist: Map[Int, Double] = classifier.outcomeDistribution(
      createDTFeatureVector(featureVector)
    )
    dist map { case (transitionIndex, prob) => (transitions(transitionIndex), prob) }

    /*
    val categoryCounts: Map[Int, Double] = decisionTree.categoryCounts(
      createDTFeatureVector(featureVector)) mapValues { _.toDouble }
    val defaultCount: Double = 0.1
    val smoothedCategoryCounts = (transitions.zipWithIndex map { case (transition, transitionIndex) =>
      val categoryCount: Double = categoryCounts.getOrElse(transitionIndex, defaultCount)
      (transition, categoryCount)
    }).toMap
    val normalizer: Double = smoothedCategoryCounts.values.sum
    val result = smoothedCategoryCounts mapValues { case count =>
      count / normalizer
    }
    result
    */
  }

  private def createDTFeatureVector(featureVector: FeatureVector): DTFeatureVector = {
    val trueAttributeNames: Seq[FeatureName] =
      featureVector.values filter { _._2 != 0 } map { _._1 }
    val trueAttributes: HashSet[Int] =
      HashSet(trueAttributeNames
        .filter(featureNameToIndex.contains)
        .map(featureNameToIndex).toSeq: _*)
    new SparseVector(None, numFeatures, trueAttributes)
  }
}

/** The [[DTCostFunctionTrainer]] uses the our in-house decision tree implementation
  * (org.allenai.nlpstack.parse.poly.decisiontree) to train a [[StateCostFunction]]. Training is
  * triggered during construction, after which the .costFunction field contains the trained
  * [[StateCostFunction]].
  *
  * @param trainingVectorSource a source of training vectors
  */
class DTCostFunctionTrainer(
  classifierTrainer: ProbabilisticClassifierTrainer,
  taskIdentifier: TaskIdentifier,
  transitionSystem: TransitionSystem, trainingVectorSource: FSMTrainingVectorSource,
  baseCostFunction: Option[StateCostFunction]
)
    extends StateCostFunctionTrainer(taskIdentifier, transitionSystem, trainingVectorSource) {

  override def costFunction: StateCostFunction = new ClassifierBasedCostFunction(
    taskIdentifier,
    transitionSystem, transitions, taskClassifiers.toList, featureNames, baseCostFunction
  )

  private val transitionIndices: Map[StateTransition, Int] = transitions
    .view.zipWithIndex.toMap

  private val taskClassifiers: Map[ClassificationTask, TransitionClassifier] = {
    val taskTrainingVectors: Iterator[(ClassificationTask, Iterator[FSMTrainingVector])] =
      trainingVectorSource.groupVectorIteratorsByTask

    var progressCounter = 1
    (taskTrainingVectors map {
      case (task, trainingVectorIter) => {
        val trainingVectors = trainingVectorIter.toIterable
        println(s"Task ${progressCounter} of ${trainingVectorSource.tasks.size}" +
          s"(${task.filenameFriendlyName}) has ${trainingVectors.size} training vectors")
        progressCounter += 1
        val vectors: DTFeatureVectors = createDTFeatureVectors(trainingVectors.iterator)
        println("Now training.")
        val inducedClassifier: ProbabilisticClassifier = classifierTrainer(vectors)
        val featureMap: Seq[(Int, FeatureName)] =
          featureNames.zipWithIndex filter {
            case (_, featIndex) =>
              inducedClassifier.allFeatures.contains(featIndex)
          } map {
            case (feat, featIndex) =>
              (featIndex, feat)
          }
        val result = (task, new EmbeddedClassifier(
          inducedClassifier,
          transitions, featureMap, featureNames.size
        ))
        result
      }
    }).toMap
  }

  private def createDTFeatureVectors(
    trainingVectorIter: Iterator[FSMTrainingVector]
  ): DTFeatureVectors = {

    new DTFeatureVectors((trainingVectorIter map createDTFeatureVector).toIndexedSeq)
  }

  private def createDTFeatureVector(trainingVector: FSMTrainingVector): DTFeatureVector = {
    val featureVector = trainingVector.featureVector
    val trueAttributeNames: Seq[FeatureName] =
      featureVector.values filter { _._2 != 0 } map { _._1 }
    val trueAttributes: HashSet[Int] =
      HashSet(trueAttributeNames
        .filter(featureNameToIndex.contains)
        .map(featureNameToIndex).toSeq: _*)
    new SparseVector(
      Some(transitionIndices(trainingVector.transition)),
      featureNames.size, trueAttributes
    )
  }
}
