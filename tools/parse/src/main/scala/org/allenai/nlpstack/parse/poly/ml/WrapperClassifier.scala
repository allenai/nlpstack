package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.decisiontree.{
  FeatureVector => DTFeatureVector,
  ProbabilisticClassifierTrainer,
  JustifyingProbabilisticClassifierTrainer,
  InMemoryFeatureVectorSource,
  SparseVector,
  ProbabilisticClassifier,
  JustifyingProbabilisticClassifier,
  Justification,
  DecisionTreeJustification,
  RandomForestJustification
}
import org.allenai.nlpstack.parse.poly.fsm.SimpleTask
import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.collection.immutable.HashSet

/** A WrapperClassifier wraps a ProbabilisticClassifier (which uses integer-based feature
  * names) in an interface that allows you to use the more natural
  * org.allenai.nlpstack.parse.poly.ml FeatureVector format for classification.
  * This is a trait that specific wrappers can extend.
  */
sealed trait WrapperClassifier {
  def classifier(): ProbabilisticClassifier
  def featureNameMap(): Seq[(Int, FeatureName)]

  /** The inverse of featureNameMap. */
  @transient
  protected val featureNameToIndex: Map[FeatureName, Int] =
    (featureNameMap map {
      case (featIndex, feat) =>
        (feat, featIndex)
    }).toMap

  /** Returns a feature name based on an index. */
  protected def featureNameForIndex(index: Int): FeatureName = {
    val map = featureNameMap.toMap
    map(index)
  }

  /** Returns the most probable (integer) outcome, given the input feature vector.
    *
    * @param featureVector the feature vector to classify
    * @return the most probable (integer) outcome
    */
  def classify(featureVector: FeatureVector): Int = {
    classifier.classify(
      WrapperClassifier.createDTFeatureVector(featureVector, featureNameToIndex, None)
    )
  }

  /** Returns a distribution over all (integer) outcomes, given the input feature vector.
    *
    * @param featureVector the feature vector to classify
    * @return the most probable (integer) outcome
    */
  def getDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    classifier.outcomeDistribution(
      WrapperClassifier.createDTFeatureVector(featureVector, featureNameToIndex, None)
    )
  }
}
/* A case class extending the WrapperClassifier trait. Does not support any new functionality.
  * @param classifier the embedded classifier (which uses integer-based feature names)
  * @param featureNameMap a map from the integer feature names to their string-based form
  */
case class BasicWrapperClassifier(
    classifier: ProbabilisticClassifier,
    featureNameMap: Seq[(Int, FeatureName)]
) extends WrapperClassifier {
}

/* A case class extending the WrapperClassifier trait, that supports methods to classify and
  * provide justification for the classification.
  * @param classifier a justifying classifier (which uses integer-based feature names)
  * @param featureNameMap a map from the integer feature names to their string-based form
  */
case class JustifyingWrapperClassifier(
    classifier: JustifyingProbabilisticClassifier,
    featureNameMap: Seq[(Int, FeatureName)]
) extends WrapperClassifier {

  /** Given the input feature vector, returns the most probable (integer) outcome, along
    * with a justification for the outcome.
    *
    * @param featureVector the feature vector to classify
    * @return a tuple containing the most probable (integer) outcome and the justification
    * (a text explanation) for that outcome.
    */
  def classifyAndJustify(featureVector: FeatureVector): (Int, String) = {
    (classifier.classifyAndJustify(
      WrapperClassifier.createDTFeatureVector(featureVector, featureNameToIndex, None)
    )) match {
        case (outcome: Int, justification: DecisionTreeJustification) =>
          (outcome, prettyPrintDecisionTreeJustification(justification))
        case (outcome: Int, justification: RandomForestJustification) =>
          (outcome, prettyPrintRandomForestJustification(justification))
        case (outcome: Int, _) => (outcome, "")
      }
  }

  private def prettyPrintDecisionTreeJustification(justification: DecisionTreeJustification): String = {
    // Map the feature indexes to their feature names
    val featureTuples: Seq[(FeatureName, Int)] = justification.breadCrumb.map {
      case (featureNameIx: Int, featureVal: Int) =>
        (featureNameForIndex(featureNameIx), featureVal)
    }

    // Group the tuples by the first symbol, which generally indicates the node in the
    // polytree, for e.g., "self", "parent1", "child1", "child2", etc.
    /*val featureTuplesGrouped: Map[Symbol, Seq[(String, Int)]] =
        featureTuples.groupBy(_._1.symbols.head) mapValues {
          case tuples: Seq[(FeatureName,Int)] =>
            tuples map { t => (new FeatureName(t._1.symbols.tail).toString, t._2) }
        }

      // Build the Justification string by composing justification for each type of node.
      val featureTuplesStr = (for {
        (k, v) <- featureTuplesGrouped
      } yield {
        "[ " + k.name + ": " +
         "[ " + (v map {
           case (featureNameStr: String, featureVal: Int) => featureNameStr + " = " + featureVal
         }).mkString(", ") + " ] ]"
       }).mkString(",\n")
       "[\n" + featureTuplesStr + "\n]\n"*/

    "[ " + (for {
      featureTuple <- featureTuples
    } yield {
      featureTuple._1 + " = " + featureTuple._2
    }).mkString(", ") + " ]"
  }

  private def prettyPrintRandomForestJustification(justification: RandomForestJustification): String =
    {
      "[\n" + justification.dtJustifications.map(j =>
        prettyPrintDecisionTreeJustification(j)).mkString(",\n\n") + "\n]\n"
    }

  /** Returns a distribution over all (integer) outcomes, given the input feature vector.
    *
    * @param featureVector the feature vector to classify
    * @return a tuple with the most probable (integer) outcome and the classifier justification for
    *   the outcome
    */
  def getDistributionWithJustification(featureVector: FeatureVector): Map[Int, (Double, String)] = {
    classifier.outcomeDistributionWithJustification(
      WrapperClassifier.createDTFeatureVector(featureVector, featureNameToIndex, None)
    ) map {
        case (outcome: Int, (confidence: Double, justification: DecisionTreeJustification)) =>
          (outcome, (confidence, prettyPrintDecisionTreeJustification(justification)))
        case (outcome: Int, (confidence: Double, justification: RandomForestJustification)) =>
          (outcome, (confidence, prettyPrintRandomForestJustification(justification)))
      }
  }
}

object WrapperClassifier {

  implicit object WrapperClassifierJsonFormat
      extends RootJsonFormat[WrapperClassifier] {

    implicit val basicWCformat = jsonFormat2(BasicWrapperClassifier.apply).pack("type" -> "basic")

    import ProbabilisticClassifier.ProbabilisticClassifierJsonFormat._

    implicit val justifyingWCformat =
      jsonFormat2(JustifyingWrapperClassifier.apply).pack("type" -> "justifying")

    def write(classifier: WrapperClassifier): JsValue = classifier match {
      case basic: BasicWrapperClassifier => basic.toJson
      case justifying: JustifyingWrapperClassifier => justifying.toJson
      case x => deserializationError(s"Cannot serialize this classifier type: $x")
    }

    def read(value: JsValue): WrapperClassifier = {
      value.asJsObject.unpackWith(basicWCformat, justifyingWCformat)
    }
  }

  /** Converts a string-based feature vector into an integer-based feature vector.
    *
    * @param featureVector the string-based feature vector
    * @param featureNameToIndex a map from string-based feature names to integers
    * @param outcome the outcome (if known) of the feature vector
    * @return the equivalent integer-based feature vector
    */
  def createDTFeatureVector(
    featureVector: FeatureVector,
    featureNameToIndex: Map[FeatureName, Int],
    outcome: Option[Int]
  ): DTFeatureVector = {
    val trueAttributeNames: Seq[FeatureName] =
      featureVector.values filter { _._2 != 0 } map { _._1 }
    val trueAttributes: HashSet[Int] =
      HashSet(trueAttributeNames
        .filter(featureNameToIndex.contains)
        .map(featureNameToIndex).toSeq: _*)
    new SparseVector(outcome, featureNameToIndex.values.max + 1, trueAttributes)
  }
}

/** Trains a WrapperClassifier from training data.
  *
  * @param classifierTrainer the underlying trainer to use (from the decisiontree package)
  */
class WrapperClassifierTrainer(
    classifierTrainer: ProbabilisticClassifierTrainer
) {

  def apply(trainingData: TrainingData): WrapperClassifier = {
    val featureNames: Seq[FeatureName] = trainingData.featureNames.toSeq
    val featureNameToIndex: Map[FeatureName, Int] = featureNames.zipWithIndex.toMap
    val vectors = new InMemoryFeatureVectorSource(
      (trainingData.labeledVectors map {
      case (vec, outcome) =>
        WrapperClassifier.createDTFeatureVector(vec, featureNameToIndex, Some(outcome))
    }).toIndexedSeq,
      SimpleTask("basic")
    )
    println(s"Task has ${vectors.numVectors} training vectors")
    val inducedClassifier: ProbabilisticClassifier = classifierTrainer(vectors)
    val featureMap: Seq[(Int, FeatureName)] =
      featureNames.zipWithIndex filter {
        case (_, featIndex) =>
          inducedClassifier.allFeatures.contains(featIndex)
      } map {
        _.swap
      }
    inducedClassifier match {
      case c: JustifyingProbabilisticClassifier =>
        new JustifyingWrapperClassifier(
          inducedClassifier.asInstanceOf[JustifyingProbabilisticClassifier], featureMap
        )
      case _ =>
        BasicWrapperClassifier(inducedClassifier, featureMap)

    }
  }
}

