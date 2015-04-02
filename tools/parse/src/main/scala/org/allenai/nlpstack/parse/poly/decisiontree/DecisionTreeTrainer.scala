package org.allenai.nlpstack.parse.poly.decisiontree

import scala.collection.mutable
import scala.util.Random

/** Internal class of DecisionTreeTrainer.
  *
  * @param data a pointer to a large indexed set of feature vectors
  * @param featureVectorSubset the subset of feature vector indices that interests us
  * @param featureSubset the subset of feature indices to consider
  */
private class Node(data: Option[FeatureVectorSource], val featureVectorSubset: Seq[Int],
    val featureSubset: Seq[Int], val depth: Int) {

  // The feature to split on, at this node.
  private var splittingFeature: Option[Int] = None

  // Maps each possible value of the splittingFeature to a child node
  private val childrenMap = mutable.HashMap[Int, Node]()

  // Memoizes the most probable outcome at this node (after .outcome has already been called).
  private var memoizedOutcome: Option[Int] = None

  // Maps each outcome to the number of validation vectors assigned that outcome.
  private val validationCounts = mutable.HashMap[Int, Int]().withDefaultValue(0)

  /** The children of this node. */
  def children: Iterable[Node] = {
    childrenMap.values
  }

  /** An iterator over all descendants of this node (including itself). */
  def nodes: Iterable[Node] = {
    Iterable(this) ++ (children flatMap { (n: Node) => n.nodes })
  }

  /** The most probable outcome at this node. As a side effect, this sets the memoizedOutcome field
    * if currently unset.
    */
  def outcome: Int = {
    require(data.isDefined)
    memoizedOutcome getOrElse {
      memoizedOutcome = Some(outcomeCounts.toList.maxBy(_._2)._1)
      memoizedOutcome.get
    }
  }

  /** Returns whether there is a child corresponding to the argument feature vector.
    *
    * @param featureVector feature vector to find child of
    * @return whether the child exists
    */
  def hasNext(featureVector: FeatureVector): Boolean = {
    if (isLeaf) {
      false
    } else {
      val featVal = featureVector.getFeature(splittingFeature.get)
      childrenMap.contains(featVal)
    }
  }

  /** Returns the child corresponding to the argument feature vector.
    *
    * Requires hasNext(featureVector)
    *
    * @param featureVector feature vector to find child of
    */
  def next(featureVector: FeatureVector): Node = {
    require(hasNext(featureVector))
    val featVal = featureVector.getFeature(splittingFeature.get)
    childrenMap(featVal)
  }

  /** Returns whether the node is a leaf. */
  def isLeaf: Boolean = {
    childrenMap.isEmpty
  }

  /** Returns the outcome counts for this node.
    *
    * @return histogram of outcomes according to training data
    */
  def outcomeCounts: Map[Int, Int] = {
    require(data.isDefined)
    featureVectorSubset.groupBy(data.get.getNthVector(_).outcome.get) mapValues {
      l => l.size
    }
  }

  /** Setter for the splitting feature.
    *
    * @param feature which feature to split on
    */
  def setFeature(feature: Int): Unit = {
    splittingFeature = Some(feature)
  }

  /** Adds (or replaces) the child node corresponding to the specified value of the splitting
    * feature.
    *
    * @param featureValue key value of child for splitting feature
    * @param child child to add
    */
  def addChild(featureValue: Int, child: Node): Unit = {
    childrenMap(featureValue) = child
  }

  def toDecisionTree(outcomes: Iterable[Int] = outcomeCounts.keys): DecisionTree = {
    val nodeToId: Map[Node, Int] = nodes.zipWithIndex.toMap
    val dtChild: IndexedSeq[Map[Int, Int]] = nodes.toIndexedSeq map { node =>
      node.childrenMap.toMap mapValues { c => nodeToId(c) }
    }
    val dtSplittingFeature: IndexedSeq[Option[Int]] = nodes.toIndexedSeq map { node =>
      node.splittingFeature
    }
    val dtOutcomeCounts: IndexedSeq[Map[Int, Int]] = nodes.toIndexedSeq map { node =>
      node.outcomeCounts map {
        case (k, v) =>
          k -> (v + node.outcomeCounts.getOrElse(k, 0))
      }
    }
    new DecisionTree(outcomes, dtChild, dtSplittingFeature, dtOutcomeCounts)
  }

  /** Stores a validation vector at this node.
    *
    * @param validationVector the feature vector to store
    */
  def addValidationVector(validationVector: FeatureVector): Unit = {
    require(validationVector.outcome.isDefined)
    val validationVectorOutcome: Int = validationVector.outcome.get
    validationCounts(validationVectorOutcome) += 1
  }

  /** Removes all children from this node. */
  def prune(): Unit = {
    splittingFeature = None
    childrenMap.clear()
  }

  /** Computes the difference between the number of validation vectors that are correctly
    * classified in the pruned tree versus the unpruned tree.
    */
  def pruningImprovement: Int = numCorrectValidationVectorsIfPruned - numCorrectValidationVectors

  private def numCorrectValidationVectorsIfPruned: Int = {
    validationCounts(outcome)
  }

  private var numCorrectValidationVectorsMemoized: Option[Int] = None

  private def numCorrectValidationVectors: Int = {
    numCorrectValidationVectorsMemoized match {
      case None => {
        val result = {
          if (isLeaf) {
            numCorrectValidationVectorsIfPruned
          } else {
            // FIXME: occasionally produces stack overflow
            (children map { (n: Node) => n.numCorrectValidationVectors }).sum
          }
        }
        numCorrectValidationVectorsMemoized = Some(result)
        result
      }
      case Some(result) => result
    }
  }
}

sealed trait InformationGainMetric {
  val minimumGain: Double
}
case class EntropyGainMetric(minimumGain: Double) extends InformationGainMetric
case class MultinomialGainMetric(minimumGain: Double) extends InformationGainMetric

/** A DecisionTreeTrainer trains decision trees from data.
  *
  * @param validationPercentage the percentage of data to "hold out" for pruning
  * @param informationGainMetric the information gain metric to use ("entropy" or "multinomial")
  * @param featuresExaminedPerNode for each node, the fraction of features
  * to randomly consider as potential splitting features
  * @param maximumDepth the maximum desired depth of the trained decision tree
  */
class DecisionTreeTrainer(
    validationPercentage: Double,
    informationGainMetric: InformationGainMetric,
    featuresExaminedPerNode: Double = 1.0,
    maximumDepth: Int = Integer.MAX_VALUE
) extends ProbabilisticClassifierTrainer {

  require(
    featuresExaminedPerNode >= 0 && featuresExaminedPerNode <= 1,
    s"featuresExaminedPerNode = $featuresExaminedPerNode, which is not between 0 and 1"
  )

  /** Factory constructor of DecisionTree.
    *
    * Randomly splits data into training:validation (according to the validationPercentage param).
    * Does reduced-error pruning on validation data.
    * Uses a uniform prior over training labels where each label is assumed to have been
    * seen once already. This Laplace smoothing affects the probability distribution over labels
    * for each feature.
    *
    * @param data training and validation data
    * @return the induced decision tree
    */
  override def apply(data: FeatureVectorSource): ProbabilisticClassifier = {
    val (trainingSubset, validationSubset) = {
      val n = data.numVectors
      val nTrain = Math.round(n.toFloat * validationPercentage).toInt
      val shuffledFeatures = Random.shuffle((0 to n - 1).toIndexedSeq)
      (shuffledFeatures.drop(nTrain), shuffledFeatures.take(nTrain))
    }
    val root = growTree(data, trainingSubset, (featuresExaminedPerNode * data.numFeatures).toInt)
    if (validationSubset.nonEmpty) {
      pruneTree(data, validationSubset, root)
    }
    root.toDecisionTree()
  }

  /** Grows a decision tree from a set of labeled feature vectors, using information gain as
    * the metric by which we split each node.
    *
    * @param data a pointer to a large indexed set of feature vectors
    * @param featureVectorSubset the subset of feature vector indices that interest us
    * @param featuresExaminedPerNode number of random sampled features for which we measure
    * information gain at each node
    * @return the final decision tree (before any pruning occurs)
    */
  private def growTree(data: FeatureVectorSource, featureVectorSubset: Seq[Int],
    featuresExaminedPerNode: Int): Node = {

    val root = new Node(Some(data), featureVectorSubset,
      data.getFeatures.toSeq, depth = 0)
    println(s"Training decision tree using ${data.numVectors} training " +
      s"vectors with ${root.featureSubset.size} features.")
    val stack = mutable.Stack[Node]()
    stack.push(root)
    while (stack.nonEmpty) {
      // nodes on the stack are newly created
      val node = stack.pop()
      // detects termination conditions: if no more features are available to split on, or
      // all remaining feature vectors are labeled with the same outcome
      if (!(node.featureSubset.isEmpty ||
        node.depth > maximumDepth ||
        node.featureVectorSubset.forall(data.getNthVector(_).outcome ==
          data.getNthVector(node.featureVectorSubset.head).outcome))) {

        val featuresToExamine = {
          val shuffled = Random.shuffle(node.featureSubset)
          shuffled.take(featuresExaminedPerNode)
        }

        // Computes information gain of each selected feature.
        // Can be expensive.
        val infoGainByFeature: Seq[(Int, Double)] = (informationGainMetric match {
          case EntropyGainMetric(_) =>
            computeEntropyBasedInformationGain(
              data, node.featureVectorSubset, featuresToExamine
            )
          case MultinomialGainMetric(_) =>
            computeMultinomialBasedInformationGain(
              data, node.featureVectorSubset, featuresToExamine
            )
        }) filter {
          case (_, gain) =>
            gain > informationGainMetric.minimumGain // get rid of features with low info gain
        }

        if (infoGainByFeature.nonEmpty) {
          val (bestFeature, _) = infoGainByFeature maxBy { case (_, gain) => gain }
          node.setFeature(bestFeature)
          val subsets = node.featureVectorSubset groupBy {
            j => data.getNthVector(j).getFeature(bestFeature)
          }
          val childFeatureSubset = node.featureSubset diff Seq(bestFeature)
          for ((featVal, childFeatureVectorSubset) <- subsets) {
            val child = new Node(Some(data), childFeatureVectorSubset, childFeatureSubset,
              node.depth + 1)
            node.addChild(featVal, child)
            stack.push(child)
          }
        }
      }
    }
    root
  }

  /** Prunes a decision tree using a set of validation feature vectors.
    *
    * This mutates the argument `root`, which will point to the pruned decision tree after
    * this function is called.
    *
    * @param data a pointer to a large indexed set of feature vectors
    * @param featureVectorSubset the subset of feature vector indices that interest us
    * @param root the root of the decision tree we want to prune
    */
  private def pruneTree(data: FeatureVectorSource, featureVectorSubset: Seq[Int], root: Node) {
    // distributes validation data to the relevant decision tree nodes
    for (vectorIndex <- featureVectorSubset) {
      val featureVector = data.getNthVector(vectorIndex)
      var cur = root
      cur.addValidationVector(featureVector)
      while (cur.hasNext(featureVector)) {
        cur = cur.next(featureVector)
        cur.addValidationVector(featureVector)
      }
    }
    // create a reverse topological order over nodes
    val stack = mutable.Stack[Node]()
    val topoList = mutable.Stack[Node]()
    stack.push(root)
    while (stack.nonEmpty) {
      val node = stack.pop()
      topoList.push(node)
      for (child <- node.children) {
        stack.push(child)
      }
    }
    // do a reverse topological search, pruning nodes with a positive pruning heuristic
    while (topoList.nonEmpty) {
      val node = topoList.pop()
      if (node.pruningImprovement > 0) {
        node.prune()
      }
    }
  }

  /** Computes the standard entropy-based information gain metric for decision tree growing.
    *
    * @param data a source of feature vectors
    * @param featureVectorSubset the indices of the feature vectors that interest us
    * @param featureSubset the indices of the features that interest us
    * @return a sequence of pairs (FEATURE, GAIN), where GAIN is FEATURE's information gain
    */
  private def computeEntropyBasedInformationGain(
    data: FeatureVectorSource,
    featureVectorSubset: Seq[Int],
    featureSubset: Seq[Int]
  ): Seq[(Int, Double)] = {

    def computeOutcomeEntropy(featureVectorSubstream: Seq[FeatureVector]) = {
      def computeEntropy(histogram: Map[Int, Int]) = {
        val frequencies = histogram.values
        val unnormalizedEntropy = (frequencies map { freq =>
          freq.toDouble * math.log(freq)
        }).sum
        val normalizer = frequencies.sum
        math.log(normalizer) - ((1.0 / normalizer) * unnormalizedEntropy)
      }
      val outcomeHistogram: Map[Int, Int] =
        featureVectorSubstream groupBy { _.outcome.get } mapValues { _.size }
      computeEntropy(outcomeHistogram)
    }

    require(featureVectorSubset.nonEmpty)
    require(featureSubset.nonEmpty)
    val featureVectorSubstream: Seq[FeatureVector] = featureVectorSubset map data.getNthVector
    val unsplitEntropy = computeOutcomeEntropy(featureVectorSubstream)
    val informationGainByFeatureValue: Iterable[Double] = for {
      feature <- featureSubset
    } yield {
      val vectorsByFeatureValue: Map[Int, Seq[FeatureVector]] =
        featureVectorSubstream groupBy { x => x.getFeature(feature) }
      unsplitEntropy - ((vectorsByFeatureValue.values map { x =>
        x.size * computeOutcomeEntropy(x)
      }).sum / featureVectorSubset.size)
    }
    featureSubset.zip(informationGainByFeatureValue)
  }

  /** Computes a multinomial-based information gain metric for decision tree growing.
    *
    * Essentially, this rewards partitions such that each partition is unlikely to be generated
    * from the base multinoulli distribution, according to the multinomial distribution.
    *
    * @param data a source of feature vectors
    * @param featureVectorSubset the indices of the feature vectors that interest us
    * @param featureSubset the indices of the features that interest us
    * @return a sequence of pairs (FEATURE, GAIN), where GAIN is FEATURE's information gain
    */
  private def computeMultinomialBasedInformationGain(
    data: FeatureVectorSource,
    featureVectorSubset: Seq[Int],
    featureSubset: Seq[Int]
  ): Seq[(Int, Double)] = {

    def getEmpiricalOutcomeDistribution(
      featureVectorSubstream: Seq[FeatureVector]
    ): Map[Int, Double] = {
      val outcomes: Seq[Int] = featureVectorSubstream flatMap { featureVec => featureVec.outcome }
      val outcomeHistogram: Map[Int, Int] = outcomes groupBy { x => x } mapValues { _.size }
      val numOutcomes = outcomes.size
      require(numOutcomes > 0, "Cannot compute an empirical distribution on an empty sequence")
      outcomeHistogram mapValues { outcomeCount =>
        outcomeCount.toFloat / numOutcomes
      }
    }

    def computeMultinomialNegativeLogProbability(
      multinoulli: Map[Int, Double],
      histogram: Map[Int, Int]
    ): Double = {
      val n = histogram.values.sum
      val positiveResult = (n * math.log(n)) + (histogram map {
        case (outcome, outcomeCount) =>
          outcomeCount * (math.log(multinoulli(outcome)) - math.log(outcomeCount))
      }).sum
      -positiveResult
    }

    require(featureVectorSubset.nonEmpty)
    require(featureSubset.nonEmpty)
    val featureVectorSubstream: Seq[FeatureVector] = featureVectorSubset map data.getNthVector
    val baselineOutcomeDistribution = getEmpiricalOutcomeDistribution(featureVectorSubstream)
    val informationGainByFeatureValue: Iterable[Double] = for {
      feature <- featureSubset
    } yield {
      val outcomesByFeatureValue: Map[Int, Seq[Int]] =
        featureVectorSubstream groupBy { featureVec =>
          featureVec.getFeature(feature)
        } mapValues { featureVecs =>
          featureVecs flatMap { featureVec => featureVec.outcome }
        }
      val outcomeHistograms: Seq[Map[Int, Int]] =
        (outcomesByFeatureValue.values map { outcomes =>
          outcomes groupBy { x => x } mapValues { _.size }
        }).toSeq
      (outcomeHistograms map { outcomeHistogram =>
        computeMultinomialNegativeLogProbability(baselineOutcomeDistribution, outcomeHistogram)
      }).sum
    }
    featureSubset.zip(informationGainByFeatureValue)
  }
}
