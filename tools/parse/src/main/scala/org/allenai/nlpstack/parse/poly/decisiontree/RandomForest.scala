package org.allenai.nlpstack.parse.poly.decisiontree

import java.io.{ PrintWriter, File }

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.fsm.{ TransitionClassifier, ClassificationTask }
import spray.json.DefaultJsonProtocol._
import spray.json._
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

/** A RandomForest is a collection of decision trees. Each decision tree gets a single vote
  * about the outcome. The outcome distribution is the normalized histogram of the votes.
  *
  * @param allOutcomes the collection of possible outcomes
  * @param decisionTrees the collection of decision trees
  */
case class RandomForest(allOutcomes: Seq[Int], decisionTrees: Seq[DecisionTree])
    extends ProbabilisticClassifier {

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
  * @param featuresExaminedPerNode during decision tree induction, desired percentage of randomly
  * selected features to consider at each node
  */
class RandomForestTrainer(validationPercentage: Double, numDecisionTrees: Int,
  featuresExaminedPerNode: Double, gainMetric: InformationGainMetric, useBagging: Boolean = false,
  maximumDepthPerTree: Int = Integer.MAX_VALUE, numThreads: Int = 1)
    extends ProbabilisticClassifierTrainer {

  require(
    featuresExaminedPerNode >= 0 && featuresExaminedPerNode <= 1,
    s"featuresExaminedPerNode = $featuresExaminedPerNode, which is not between 0 and 1"
  )

  private val dtTrainer = new DecisionTreeTrainer(validationPercentage, gainMetric,
    featuresExaminedPerNode, maximumDepth = maximumDepthPerTree)

  /** Induces a RandomForest from a set of feature vectors.
    *
    * @param data a set of feature vectors to use for training
    * @return the induced random forest
    */
  override def apply(data: FeatureVectorSource): ProbabilisticClassifier = {
    import scala.concurrent.ExecutionContext.Implicits.global
    System.setProperty("scala.concurrent.context.numThreads", numThreads.toString)
    val tasks: Seq[Future[File]] = for (i <- Range(0, numDecisionTrees)) yield Future {
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
    val futureSubtreeFiles: Future[Seq[File]] = Future.sequence(tasks)
    val subtreeFiles = Await.result(futureSubtreeFiles, 30 days)
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
    System.clearProperty("scala.concurrent.context.numThreads")
    RandomForest(data.allOutcomes, subtrees)
  }
}
