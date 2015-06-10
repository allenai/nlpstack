package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.fsm.{ TransitionClassifier, ClassificationTask }
import org.allenai.nlpstack.parse.poly.ml.FeatureName

import reming.CompactPrinter
import reming.DefaultJsonProtocol._

import java.io.{ BufferedWriter, File, FileWriter, PrintWriter }

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

case class RandomForestJustification(
    randomForest: RandomForest, tree: Int, treeNode: Int
) extends Justification {

  def prettyPrint(featureNames: Map[Int, FeatureName]): String = {
    val dtJustification = DecisionTreeJustification(randomForest.decisionTrees(tree), treeNode)
    dtJustification.prettyPrint(featureNames)
  }
}

/** A RandomForest is a collection of decision trees. Each decision tree gets a single vote
  * about the outcome. The outcome distribution is the normalized histogram of the votes.
  *
  * @param allOutcomes the collection of possible outcomes
  * @param decisionTrees the collection of decision trees
  */
case class RandomForest(allOutcomes: Seq[Int], decisionTrees: Seq[DecisionTree])
    extends ProbabilisticClassifier {

  require(decisionTrees.nonEmpty, "Cannot initialize a RandomForest with zero decision trees")

  /** Each decision gets a vote (i.e. suggests a distribution) about the outcome. The produced
    * distribution is the normalized sum of the votes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return a probability distribution over outcomes
    */
  override def outcomeDistribution(
    featureVector: FeatureVector
  ): (OutcomeDistribution, Option[Justification]) = {

    val decisionTreeDistributions = decisionTrees map { decisionTree =>
      decisionTree.outcomeDistribution(featureVector)._1
    }
    val unnormalizedOutcomeDistribution = OutcomeDistribution.sum(decisionTreeDistributions)
    val normalizedOutcomeDistribution = OutcomeDistribution(
      ProbabilisticClassifier.normalizeDistribution(
      unnormalizedOutcomeDistribution.dist.toSeq
    ).toMap
    )
    (normalizedOutcomeDistribution, None)
  }

  /** The set of all features found in at least one decision tree of the collection. */
  override def allFeatures: Set[Int] = {
    (decisionTrees map { _.allFeatures }) reduce { (x, y) => x ++ y }
  }
}

object RandomForest {
  implicit val rfFormat = jsonFormat2(RandomForest.apply)
}

/** A RandomForestTrainer trains a RandomForest from a set of feature vectors.
  *
  * @param validationPercentage percentage of feature vectors to hold out for decision tree
  * validation
  * @param numDecisionTrees desired number of decision trees in the forest
  * @param featuresExaminedPerNode during decision tree induction, desired percentage of randomly
  * selected features to consider at each node
  */
class RandomForestTrainer(validationPercentage: Float, numDecisionTrees: Int,
  featuresExaminedPerNode: Float, gainMetric: InformationGainMetric, useBagging: Boolean = false,
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
          Resource.using(new PrintWriter(new BufferedWriter(new FileWriter(tempFile)))) { writer =>
            writer.println(CompactPrinter.printTo(writer, dt))
          }
          tempFile
      }
    }
    val futureSubtreeFiles: Future[Seq[File]] = Future.sequence(tasks)
    val subtreeFiles = Await.result(futureSubtreeFiles, 30 days)
    val subtrees: Seq[DecisionTree] = subtreeFiles map {
      case subtreeFile => Util.readFromUrl[DecisionTree](subtreeFile.toURI.toURL)
    }
    System.clearProperty("scala.concurrent.context.numThreads")
    RandomForest(data.allOutcomes, subtrees)
  }
}
