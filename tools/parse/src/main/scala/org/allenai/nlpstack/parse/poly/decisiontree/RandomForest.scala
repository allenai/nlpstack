package org.allenai.nlpstack.parse.poly.decisiontree

import java.io.{ PrintWriter, File }

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.fsm.{ TransitionClassifier, ClassificationTask }
import spray.json.DefaultJsonProtocol._
import spray.json._

/** Random Forest outcomes can be explained in terms of the individual decision trees'
  * justifications.
  * @param dtJustifications contains individual decision trees' justifications: this may be a pruned
  * list of justifications based on the chosen cutoff for top n decision trees-- currently not a
  * tunable parameter but hardcoded in outcomeDistributionWithJustification to 1, which will result
  * in only the top scoring decision tree's justification being bubbled up here.
  * @param dtCountForOutcome no. of decision trees that voted for the outcome associated with this
  * justification. (A classifier outcome will have a justification associated with it -- refer to
  * the return type of the classifyAndJustify method, for e.g.).
  * @param totalDtCount total no. of decision trees in the random forest.
  */
case class RandomForestJustification(
  decisionTreeJustifications: Seq[DecisionTreeJustification],
  decisionTreeCountForOutcome: Int, totalDecisionTreeCount: Int
) extends Justification

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
    * @return a probability distribution over outcomes, and justifications for the outcomes.
    */
  override def outcomeDistributionWithJustification(
    featureVector: FeatureVector
  ): Map[Int, (Double, RandomForestJustification)] = {
    // Call 'classifyAndJustify' on each decision tree in the forest, group by outcomes, and
    // aggregate total counts of the number of trees producing each outcome, and aggregate
    // corresponding justifications from the individual decision trees.
    val outcomeHistogram =
      (for {
        decisionTree <- decisionTrees
      } yield {
        val outcome = decisionTree.classifyAndJustify(featureVector)
        val dtJustification = outcome._2 match {
          case justification: DecisionTreeJustification =>
            justification.asInstanceOf[DecisionTreeJustification]
        }
        val nodeScore = decisionTree.getNodeDivergenceScore(dtJustification.node)
        (outcome._1, dtJustification, nodeScore)
      }).groupBy { x => x._1 }
    val outcomeHistogramWithDtJustifications: Map[Int, (Int, RandomForestJustification)] =
      aggregateOutcomeCountsAndJustifications(outcomeHistogram, Some(1), decisionTrees.size)
    RandomForest.normalizeHistogram(
      outcomeHistogramWithDtJustifications.map { case (k, v) => (k, v._1) }
    ).map {
        case (k, v) => (k, (v, outcomeHistogramWithDtJustifications(k)._2))
      }
  }

  /* Aggregate counts for each outcome (by summing them up), and aggregate the
   * decision tree justifications by creating a RandomForestJustification out of them.
   * 
   * @param outcomeHistogram  a map of each outcome to a seq of indivdual occurrences of that
   * outcome, with from various decision trees, the decision tree justification for each,
   * and the score for the decision tree node chosen for each case.
   * @param numDecisionTreesToConsiderForJustification  an optional integer specifying a number
   * for the top n decision tree justifications to propagate to the random forest justification. The
   * justifications are ranked by the associated node's score. If this parameter is unspecified, all
   * decision tree justifications are bubbled up to the random forest justification.
   * @param totalDtCount total no. of decision trees in the random forest- used to bubble up to 
   * the random forest justification that is created here and returned.
   */
  private def aggregateOutcomeCountsAndJustifications(
    outcomeHistogram: Map[Int, Seq[(Int, DecisionTreeJustification, Double)]],
    numDecisionTreesToConsiderForJustification: Option[Int] = None,
    totalDtCount: Int
  ): Map[Int, (Int, RandomForestJustification)] = {
    (outcomeHistogram mapValues { v =>
      (v.size, v)
    }) mapValues {
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
