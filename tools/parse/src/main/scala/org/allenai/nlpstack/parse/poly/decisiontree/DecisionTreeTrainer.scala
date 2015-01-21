package org.allenai.nlpstack.parse.poly.decisiontree

import scala.collection.mutable
import scala.util.Random

/** Internal class of DecisionTreeTrainer.
  *
  * @param data a pointer to a large indexed set of feature vectors
  * @param featureVectorSubset the subset of feature vector indices that interests us
  * @param featureSubset the subset of feature indices to consider
  */
private class Node(private var data: Option[FeatureVectors], val featureVectorSubset: Seq[Int],
    val featureSubset: Seq[Int]) {

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
    featureVectorSubset.groupBy(data.get.featureVectors(_).outcome.get) mapValues {
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
    val dtChild: IndexedSeq[Seq[(Int, Int)]] = nodes.toIndexedSeq map { node =>
      (node.childrenMap.toMap mapValues { c => nodeToId(c) }).toSeq
    }
    val dtSplittingFeature: IndexedSeq[Option[Int]] = nodes.toIndexedSeq map { node =>
      node.splittingFeature
    }
    val dtOutcomeCounts: IndexedSeq[Seq[(Int, Int)]] = nodes.toIndexedSeq map { node =>
      (node.outcomeCounts map {
        case (k, v) =>
          k -> (v + node.outcomeCounts.getOrElse(k, 0))
      }).toSeq
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

/** Functions for training decision trees. */
//param validationPercentage percentage of the training vectors to hold out for validation
class DecisionTreeTrainer(
    validationPercentage: Double,
    featuresExaminedPerNode: Int = Integer.MAX_VALUE
) extends ProbabilisticClassifierTrainer {

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
  override def apply(data: FeatureVectors): ProbabilisticClassifier = {

    val (trainingSubset, validationSubset) = {
      val n = data.numVectors
      val nTrain = Math.round(n.toFloat * validationPercentage).toInt
      val shuffledFeatures = Random.shuffle((0 to n - 1).toIndexedSeq)
      (shuffledFeatures.drop(nTrain), shuffledFeatures.take(nTrain))
    }
    val root = growTree(data, trainingSubset, featuresExaminedPerNode)
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
  private def growTree(data: FeatureVectors, featureVectorSubset: Seq[Int],
    featuresExaminedPerNode: Int): Node = {

    val root = new Node(Some(data), featureVectorSubset,
      (0 to data.numFeatures - 1).toIndexedSeq)
    val stack = mutable.Stack[Node]()
    stack.push(root)
    while (stack.nonEmpty) {
      // nodes on the stack are newly created
      val node = stack.pop()
      // detects termination conditions: if no more features are available to split on, or
      // all remaining feature vectors are labeled with the same outcome
      if (!(node.featureSubset.isEmpty ||
        node.featureVectorSubset.forall(data.featureVectors(_).outcome ==
          data.featureVectors(node.featureVectorSubset.head).outcome))) {

        val (featuresToExamine, featuresToIgnore) = {
          val shuffled = Random.shuffle(node.featureSubset)
          (shuffled.take(featuresExaminedPerNode), shuffled.drop(featuresExaminedPerNode))
        }

        // can be expensive
        val infoGainByFeature: Seq[(Int, Double)] =
          computeInformationGainUnoptimized(data, node.featureVectorSubset, featuresToExamine) filter {
            case (_, gain) => gain > 0 // we get rid of features with zero information gain
          }

        if (infoGainByFeature.nonEmpty) {
          val (bestFeature, _) = infoGainByFeature maxBy { case (_, gain) => gain }
          node.setFeature(bestFeature)
          val subsets = node.featureVectorSubset groupBy {
            j => data.featureVectors(j).getFeature(bestFeature)
          }
          val childFeatureSubset = {
            val nonzeroFeatures = infoGainByFeature map { case (feature, _) => feature }
            val bestFeatureIndex = nonzeroFeatures.indexOf(bestFeature)
            featuresToIgnore ++ nonzeroFeatures.take(bestFeatureIndex) ++
              nonzeroFeatures.drop(bestFeatureIndex + 1)
          }
          for ((featVal, childFeatureVectorSubset) <- subsets) {
            val child = new Node(Some(data), childFeatureVectorSubset, childFeatureSubset)
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
  private def pruneTree(data: FeatureVectors, featureVectorSubset: Seq[Int], root: Node) {
    // distributes validation data to the relevant decision tree nodes
    for (vectorIndex <- featureVectorSubset) {
      val featureVector = data.featureVectors(vectorIndex)
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

  private def computeInformationGainUnoptimized(data: FeatureVectors, featureVectorSubset: Seq[Int],
    featureSubset: Seq[Int]): Seq[(Int, Double)] = {

    require(featureVectorSubset.nonEmpty)
    require(featureSubset.nonEmpty)

    val featureVectorSubstream: Seq[FeatureVector] = featureVectorSubset map {
      data.featureVectors(_)
    }
    val unsplitEntropy = computeOutcomeEntropy(featureVectorSubstream)
    val informationGainByFeatureValue: Iterable[Double] = for {
      feature <- featureSubset
    } yield {
      val vectorsByFeatureValue: Map[Int, Seq[FeatureVector]] =
        featureVectorSubstream groupBy { x => x.getFeature(feature) }
      val result = unsplitEntropy - ((vectorsByFeatureValue.values map { x =>
        x.size * computeOutcomeEntropy(x)
      }).sum / featureVectorSubset.size)
      result
    }
    featureSubset.zip(informationGainByFeatureValue)
  }

  /** Computes the entropy of the outcome distribution of a set of feature vectors.
    *
    * The argument provides a stream of FeatureVector objects, each of which is labeled with an
    * outcome. The normalized histogram of the outcome frequencies is a probability distribution.
    * This function returns the entropy of that distribution.
    *
    * @param featureVectorSubstream the sequence of feature vectors that interest us
    * @return the entropy of the outcome distribution, as described above
    */
  private def computeOutcomeEntropy(featureVectorSubstream: Seq[FeatureVector]) = {
    val outcomeHistogram: Map[Int, Int] =
      featureVectorSubstream groupBy { _.outcome.get } mapValues { _.size }
    computeEntropy(outcomeHistogram)
  }

  private def computeEntropy(histogram: Map[Int, Int]) = {
    val frequencies = histogram.values
    val unnormalizedEntropy = (frequencies map { freq =>
      freq.toDouble * math.log(freq)
    }).sum
    val normalizer = frequencies.sum
    math.log(normalizer) - ((1.0 / normalizer) * unnormalizedEntropy)
  }
}

