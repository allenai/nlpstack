package org.allenai.nlpstack.parse.poly.decisiontree

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.fsm.{ TransitionClassifier, ClassificationTask }

import reming.CompactPrinter
import reming.DefaultJsonProtocol._

import java.io.{ BufferedWriter, File, FileWriter, PrintWriter }

import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

case class RandomForestJustification(
  tree: Int, treeNode: Int
) extends Justification

/** A RandomForest is a collection of decision trees. Each decision tree gets a single vote
  * about the outcome. The outcome distribution is the normalized histogram of the votes.
  *
  * @param allOutcomes the collection of possible outcomes
  * @param decisionTrees the collection of decision trees
  */
case class RandomForest(allOutcomes: Seq[Int], decisionTrees: Seq[DecisionTree])
    extends ProbabilisticClassifier {

  require(decisionTrees.nonEmpty, "Cannot initialize a RandomForest with zero decision trees")

  override def classify(featureVector: FeatureVector): (Int, Option[Justification]) = {
    val decisionTreeOutputs: Seq[(Int, Option[Justification])] = decisionTrees map { decisionTree =>
      decisionTree.classify(featureVector)
    }
    val outcomeHistogram = decisionTreeOutputs map {
      _._1
    } groupBy { x =>
      x
    } mapValues { v =>
      v.size
    }
    val (bestOutcome, _) = outcomeHistogram maxBy { case (_, numVotes) => numVotes }
    val majorityJustifications: Seq[(Int, Justification)] =
      (decisionTreeOutputs.zipWithIndex filter {
        case ((outcome, _), _) =>
          outcome == bestOutcome
      } map {
        case ((_, maybeJustification), treeIndex) =>
          maybeJustification map { justification =>
            (treeIndex, justification)
          }
      }).flatten
    val justification =
      if (majorityJustifications.isEmpty) {
        None
      } else {
        val (mostConvincingTree, mostConvincingJustification) =
          majorityJustifications maxBy {
            case (treeIndex, justification) =>
              justification match {
                case dtJust: DecisionTreeJustification =>
                  decisionTrees(treeIndex).getNodeDivergenceScore(dtJust.node)
              }
          }
        val mostConvincingNode = mostConvincingJustification match {
          case dtJust: DecisionTreeJustification =>
            dtJust.node
        }
        Some(RandomForestJustification(mostConvincingTree, mostConvincingNode))
      }
    (bestOutcome, justification)
  }

  /** Each decision gets a single vote about the outcome. The produced distribution is the
    * normalized histogram of the votes.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return a probability distribution over outcomes
    */
  override def outcomeDistribution(featureVector: FeatureVector): Map[Int, Float] = {
    val outcomeHistogram = decisionTrees map { decisionTree =>
      decisionTree.classify(featureVector)
    } map {
      case (decision, _) =>
        decision
    }
    RandomForest.normalizeHistogram(outcomeHistogram groupBy { x => x } mapValues { v => v.size })
  }

  /** As in the outcomeDistribution method, returns the produced distribution as a
    * normalized histogram of the votes from individual decision trees. In addition, produces
    * a justification for each outcome, by aggregating decision tree justifications.
    *
    * param featureVector feature vector to find outcome distribution for
    * @return a probability distribution over outcomes, and justifications for the outcomes.
    */
  /*
  override def outcomeDistributionWithJustification(
    featureVector: FeatureVector
  ): Map[Int, (Float, RandomForestJustification)] = {
    // Call 'classifyAndJustify' on each decision tree in the forest, group by outcomes, and
    // aggregate total counts of the number of trees producing each outcome, and aggregate
    // corresponding justifications from the individual decision trees.
    val outcomeHistogram: Map[Int, Seq[(Int, DecisionTreeJustification, Double)]] =
      (for {
        decisionTree <- decisionTrees
      } yield {
        val outcome = decisionTree.classifyAndJustify(featureVector)
        val dtJustification = outcome._2 match {
          case justification: DecisionTreeJustification =>
            justification
        }
        val nodeScore = decisionTree.getNodeDivergenceScore(dtJustification.node)
        (outcome._1, dtJustification, nodeScore)
      }).groupBy { x => x._1 }
    val outcomeHistogramWithDtJustifications: Map[Int, (Int, RandomForestJustification)] =
      aggregateOutcomeCountsAndJustifications(outcomeHistogram, Some(1), decisionTrees.size)
    val result = RandomForest.normalizeHistogram(
      outcomeHistogramWithDtJustifications.map { case (k, v) => (k, v._1) }
    ).map {
        case (k, v) =>
          println(s"k=$k; v=$v")
          val justification: RandomForestJustification = outcomeHistogramWithDtJustifications(k)._2
          (k, (v, justification))
      }
    val maxime = result maxBy { x => x._2._1 }
    val bestJust = maxime._2._2
    println(s"Best justification: $bestJust")
    if(bestJust.decisionTreeCountForOutcome < 6) {
      println("EH OH!")
    }
    result
  }
  */

  /* Aggregate counts for each outcome, and aggregate the decision tree justifications
   * by creating a RandomForestJustification out of them.
   *
   * @param outcomeHistogram  a map of each outcome to a seq of indivdual occurrences of that
   * outcome from various decision trees, with the decision tree justification for each,
   * and the score for the decision tree node chosen for each case.
   * @param numDecisionTreesToConsiderForJustification  an optional integer specifying a number
   * for the top n decision tree justifications to propagate to the random forest justification. The
   * justifications are ranked by the associated node's score. If this parameter is unspecified, all
   * decision tree justifications are bubbled up to the random forest justification.
   * @param totalDtCount total no. of decision trees in the random forest- used to bubble up to
   * the random forest justification that is created here and returned.
   */
  /*
  private def aggregateOutcomeCountsAndJustifications(
    outcomeHistogram: Map[Int, Seq[(Int, DecisionTreeJustification, Double)]],
    numDecisionTreesToConsiderForJustification: Option[Int] = None,
    totalDtCount: Int
  ): Map[Int, (Int, RandomForestJustification)] = {

    val v: Map[Int, (Int, Seq[(Int, DecisionTreeJustification, Double)])] =
      outcomeHistogram mapValues { v =>
        (v.size, v)
      }

    v.mapValues {
      case (outcomeCount: Int,
        (dtJustificationsWithScores: Seq[(Int, DecisionTreeJustification, Double)])) =>
        // Sort Decision Tree justifications by descending order of node scores and take the top
        // n justifications to build the Random Forest Justification.
        val dtJustificationsToReport = dtJustificationsWithScores.sortBy(_._3).reverse.take(
          numDecisionTreesToConsiderForJustification.getOrElse(dtJustificationsWithScores.size)
        ).
          map(x => x._2)
        (outcomeCount,
          new RandomForestJustification(dtJustificationsToReport, outcomeCount, totalDtCount))
    }
  }
  */

  /** An experimental weighted version of the above .outcomeDistribution method.
    *
    * @param featureVector feature vector to find outcome distribution for
    * @return a probability distribution over outcomes
    */
  def outcomeDistributionAlternate(featureVector: FeatureVector): Map[Int, Float] = {
    //val outcomeHistograms: String = decisionTrees flatMap { decisionTree =>
    //  decisionTree.outcomeHistogram(featureVector).toSeq
    //}
    //outcomeHistograms map { hist => hist.}

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
  @transient lazy val decisionRules: Seq[(Seq[(Int, Int)], Float)] = {
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
  def normalizeHistogram(histogram: Map[Int, Int]): Map[Int, Float] = {
    val normalizer: Float = histogram.values.sum
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
