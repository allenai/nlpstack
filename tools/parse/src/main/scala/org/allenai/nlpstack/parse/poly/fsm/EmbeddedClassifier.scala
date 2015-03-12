package org.allenai.nlpstack.parse.poly.fsm

import java.io.{ InputStream, File, PrintWriter }

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.decisiontree.{
  FeatureVector => DTFeatureVector,
  FeatureVectorSource => DTFeatureVectorSource,
  _
}
import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, FeatureVector }

import spray.json._
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
  transitionSystem: TransitionSystem, trainingVectorSource: FSMTrainingVectorSource,
  baseCostFunction: Option[StateCostFunction]
)
    extends StateCostFunctionTrainer(transitionSystem, trainingVectorSource) {

  override def costFunction: StateCostFunction = new ClassifierBasedCostFunction(
    transitionSystem, transitions, taskClassifiers.toList, baseCostFunction
  )

  private val transitionIndices: Map[StateTransition, Int] = transitions
    .view.zipWithIndex.toMap

  private val taskClassifiers: Map[ClassificationTask, TransitionClassifier] = {
    val taskTrainingVectors: Iterator[(ClassificationTask, Iterator[FSMTrainingVector])] =
      trainingVectorSource.groupVectorIteratorsByTask

    var progressCounter = 1
    val taskClassifierSeq = taskTrainingVectors map {
      case (task, trainingVectorIter) =>
        val classifier: TransitionClassifier =
          trainClassifier(progressCounter, task, trainingVectorIter)
        val tempFile: File = File.createTempFile("temp", "ecl")
        tempFile.deleteOnExit()
        Resource.using(new PrintWriter(tempFile)) { writer =>
          writer.println(classifier.toJson.compactPrint)
        }
        progressCounter += 1
        (task, tempFile)
    }
    taskClassifierSeq.toMap mapValues {
      case classifierFile =>
        Resource.using(classifierFile.toURI.toURL.openStream()) { stream =>
          val jsVal = Util.getJsValueFromStream(stream)
          jsVal match {
            case JsObject(values) =>
            case _ => deserializationError("Unexpected JsValue type. Must be " +
              "JsObject.")
          }
          jsVal.convertTo[TransitionClassifier]
        }
    }
  }

  private def trainClassifier(progressCounter: Int, task: ClassificationTask,
    trainingVectorIter: Iterator[FSMTrainingVector]): EmbeddedClassifier = {

    println(s"Task ${progressCounter} of ${trainingVectorSource.tasks.size}")
    val vectors: DTFeatureVectorSource =
      createDTFeatureVectorSource(
        task,
        trainingVectorIter
      )
    println("Now training.")
    val inducedClassifier: ProbabilisticClassifier = classifierTrainer(vectors)
    val featureMap: Seq[(Int, FeatureName)] =
      featureNames.zipWithIndex filter {
        case (_, featIndex) =>
          inducedClassifier.allFeatures.contains(featIndex)
      } map { _.swap }
    new EmbeddedClassifier(
      inducedClassifier,
      transitions, featureMap, featureNames.size
    )
  }

  private def createDTFeatureVectorSource(
    task: ClassificationTask, trainingVectorIter: Iterator[FSMTrainingVector]
  ): DTFeatureVectorSource = {

    new InMemoryFeatureVectorSource(
      (trainingVectorIter map createDTFeatureVector).toIndexedSeq, task
    )
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
