package org.allenai.nlpstack.parse.poly.decisiontree

import java.io.{ PrintWriter, File }

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.fsm.{ TransitionClassifier, ClassificationTask }
import spray.json.DefaultJsonProtocol._
import spray.json._

/** Random Forest outcomes can be explained in terms of the individual decision trees'
  * justifications.
  * @param dtJustifications contains all individual decision trees' justifications.
  */
case class RandomForestJustification(dtJustifications: Seq[DecisionTreeJustification])
  extends Justification

/** A RandomForest is a collection of decision trees. Each decision tree gets a single vote
  * about the outcome. The outcome distribution is the normalized histogram of the votes.
  *
  * @param allOutcomes the collection of possible outcomes
  * @param decisionTrees the collection of decision trees
  */
case class RandomForest(allOutcomes: Seq[Int], decisionTrees: Seq[DecisionTree])
    extends JustifyingProbabilisticClassifier {

  require(decisionTrees.nonEmpty, "Cannot initialize a RandomForest with zero decision trees")

  /** Each decision gets a single vote about the outcome. The produced distribution is the
    * normalized histogram of the votes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return a probability distribution over outcomes
    */
  override def outcomeDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    val outcomeHistogram = decisionTrees map { decisionTree =>
      decisionTree.classify(featureVector)
    } groupBy { x => x } mapValues { v => v.size }
    RandomForest.normalizeHistogram(outcomeHistogram)
  }

  /** Each decision gets a single vote about the outcome. The produced distribution is the
    * normalized histogram of the votes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return a probability distribution over outcomes, together with explanations for the outcomes.
    */
  override def outcomeDistributionWithJustification(featureVector: FeatureVector): Map[Int, (Double, RandomForestJustification)] = {
    // Call 'classifyAndJustify' on each decision tree in the forest, group by outcomes, and
    // aggregate total counts of the number of trees producing each outcome, and aggregate
    // corresponding justifications from the individual decision trees.
    decisionTrees map {
      decisionTree => decisionTree.classifyAndJustify(featureVector)
    }
    val outcomeHistogram: Map[Int, Seq[(Int, Justification)]] =
      decisionTrees map { decisionTree => decisionTree.classifyAndJustify(featureVector)
      } groupBy { x => x._1 }
    val outcomeHistogramWithDtJustifications: Map[Int, (Int, RandomForestJustification)] =
      aggregateJustifications(outcomeHistogram)
    RandomForest.normalizeHistogram(
      outcomeHistogramWithDtJustifications.map { case (k, v) => (k, v._1) }
    ).map {
        case (k, v) => (k, (v, outcomeHistogramWithDtJustifications(k)._2))
      }
  }

  // Aggregate justifications for each outcome by performing the following steps-
  // 1. Use a fold operation to get a Seq of all justifications from all the individual decision
  //  trees.
  // 2. Convert the Seq[Justification] to a Seq[DecisionTreeJustification].
  // 3. Create a RandomForestJustification out of the Seq of DecisionTreeJustifications.
  private def aggregateJustifications(
    outcomeHistogram: Map[Int, Seq[(Int, Justification)]]
  ): Map[Int, (Int, RandomForestJustification)] = {
    (outcomeHistogram mapValues { v =>
      (v.size, v.foldLeft(Seq.empty[Justification])((dtJustifications, dtOutcomeAndJustification) =>
        dtJustifications :+ dtOutcomeAndJustification._2))
    }) mapValues {
      case (totalCount: Int, (dtJustifications: Seq[Justification])) =>
        {
          (totalCount, new RandomForestJustification(dtJustifications map {
            case justification: DecisionTreeJustification =>
              justification.asInstanceOf[DecisionTreeJustification]
          }))
        }
    }
  }

  /** An experimental weighted version of the above .outcomeDistribution method.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return a probability distribution over outcomes
    */
  def outcomeDistributionAlternate(featureVector: FeatureVector): Map[Int, Double] = {
    val summedOutcomeHistograms: Map[Int, Int] = decisionTrees flatMap { decisionTree =>
      decisionTree.outcomeHistogram(featureVector).toSeq
    } groupBy { case (x, y) => x } mapValues { case x => x map { _._2 } } mapValues { _.sum }
    RandomForest.normalizeHistogram(summedOutcomeHistograms)
  }

  /** The set of all features found in at least one decision tree of the collection. */
  override def allFeatures: Set[Int] = {
    (decisionTrees map { _.allFeatures }) reduce { (x, y) => x ++ y }
  }

  /*
  @transient lazy val decisionRules: Seq[(Seq[(Int, Int)], Double)] = {
    (decisionTrees flatMap { decisionTree =>
      decisionTree.decisionPaths zip (decisionTree.distribution map { x => x(1) })
    }).toSet.toSeq
  }
  */
}

object RandomForest {
  implicit val rfFormat = jsonFormat2(RandomForest.apply)

  /** Normalizes a histogram into a probability distribution.
    *
    * @param histogram maps each (integral valued) outcome to its count
    * @return the normalized histogram
    */
  def normalizeHistogram(histogram: Map[Int, Int]): Map[Int, Double] = {
    val normalizer: Double = histogram.values.sum
    require(normalizer > 0d)
    histogram mapValues { _ / normalizer }
  }
}

/** A RandomForestTrainer trains a RandomForest from a set of feature vectors.
  *
  * @param validationPercentage percentage of feature vectors to hold out for decision tree
  * validation
  * @param numDecisionTrees desired number of decision trees in the forest
  * @param featuresExaminedPerNode during decision tree induction, desired number of randomly
  * selected features to consider at each node
  */
class RandomForestTrainer(validationPercentage: Double, numDecisionTrees: Int,
  featuresExaminedPerNode: Int, gainMetric: InformationGainMetric, useBagging: Boolean = false,
  maximumDepthPerTree: Int = Integer.MAX_VALUE)
    extends ProbabilisticClassifierTrainer {

  private val dtTrainer = new DecisionTreeTrainer(validationPercentage, gainMetric,
    featuresExaminedPerNode, maximumDepth = maximumDepthPerTree)

  /** Induces a RandomForest from a set of feature vectors.
    *
    * @param data a set of feature vectors to use for training
    * @return the induced random forest
    */
  override def apply(data: FeatureVectorSource): ProbabilisticClassifier = {
    val subtreeFiles = Seq.fill(numDecisionTrees) {
      dtTrainer(data) match {
        case dt: DecisionTree =>
          val tempFile: File = File.createTempFile("temp.", ".dt")
          tempFile.deleteOnExit()
          Resource.using(new PrintWriter(tempFile)) { writer =>
            writer.println(dt.toJson.compactPrint)
          }
          tempFile
      }
    }
    val subtrees: Seq[DecisionTree] = subtreeFiles map {
      case subtreeFile =>
        Resource.using(subtreeFile.toURI.toURL.openStream()) { stream =>
          val jsVal = Util.getJsValueFromStream(stream)
          jsVal match {
            case JsObject(_) => jsVal.convertTo[DecisionTree]
            case _ => deserializationError("Unexpected JsValue type. Must be " +
              "JsObject.")
          }
        }
    }
    RandomForest(data.allOutcomes, subtrees)
  }
}
