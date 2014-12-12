package org.allenai.nlpstack.parse.poly.decisiontree

import scala.collection.mutable
import scala.util.Random

/** Internal class of DecisionTreeTrainer.
  *
  * @param data a pointer to a large indexed set of feature vectors
  * @param featureVectorSubset the subset of feature vector indices that interests us
  * @param attributeSubset the subset of attribute indices to consider
  */
private class Node(private var data: Option[FeatureVectors], val featureVectorSubset: Seq[Int],
    val attributeSubset: IndexedSeq[Int]) {

  // The attribute to split on, at this node.
  private var splittingAttribute: Option[Int] = None

  // Maps each possible value of the splitAttribute to a child node
  private val childrenMap = mutable.HashMap[Int, Node]()

  // Memoizes the most probable choice at this node (after .choice has already been called).
  private var memoizedChoice: Option[Int] = None

  // Maps each choice to the number of validation vectors assigned that choice.
  private val validationCounts = mutable.HashMap[Int, Int]().withDefaultValue(0)

  /** The children of this node. */
  def children: Iterable[Node] = {
    childrenMap.values
  }

  /** An iterator over all descendants of this node (including itself). */
  def nodes: Iterable[Node] = {
    Iterable(this) ++ (children flatMap { (n: Node) => n.nodes })
  }

  /** The most probable choice at this node. As a side effect, this sets the memoizedChoice field
    * if currently unset.
    */
  def choice: Int = {
    require(data.isDefined)
    memoizedChoice getOrElse {
      memoizedChoice = Some(categoryCounts.toList.maxBy(_._2)._1)
      memoizedChoice.get
    }
  }

  /** Returns whether there is a child corresponding to the argument feature vector.
    *
    * @param inst instance to find child of
    * @return whether the child exists
    */
  def hasNext(inst: FeatureVector): Boolean = {
    if (isLeaf) {
      false
    } else {
      val featVal = inst.getAttribute(splittingAttribute.get)
      childrenMap.contains(featVal)
    }
  }

  /** Returns the child corresponding to the argument feature vector.
    *
    * Requires hasNext(inst)
    *
    * @param inst instance to find child of
    */
  def next(inst: FeatureVector): Node = {
    require(hasNext(inst))
    val featVal = inst.getAttribute(splittingAttribute.get)
    childrenMap(featVal)
  }

  /** Returns whether the node is a leaf. */
  def isLeaf: Boolean = {
    childrenMap.isEmpty
  }

  /** Returns the choice counts for this node.
    *
    * @return histogram of choices according to training data
    */
  def categoryCounts: Map[Int, Int] = {
    require(data.isDefined)
    featureVectorSubset.groupBy(data.get.featureVectors(_).label.get) mapValues {
      l => l.size
    }
  }

  /** Setter for the splitting attribute.
    *
    * @param attr which attribute to split on
    */
  def setAttribute(attr: Int): Unit = {
    splittingAttribute = Some(attr)
  }

  /** Adds (or replaces) the child node corresponding to the specified value of the splitting
    * attribute.
    *
    * @param attributeValue key value of child for splitting attribute
    * @param child child to add
    */
  def addChild(attributeValue: Int, child: Node): Unit = {
    childrenMap(attributeValue) = child
  }

  def toDecisionTree(categories: Iterable[Int] = categoryCounts.keys): DecisionTree = {
    val nodeToId: Map[Node, Int] = nodes.zipWithIndex.toMap
    // add prior counts
    val priorCounts = categories.toList.map(_ -> 1).toMap
    val dtChild: IndexedSeq[Seq[(Int, Int)]] = nodes.toIndexedSeq map { node =>
      (node.childrenMap.toMap mapValues { c => nodeToId(c) }).toSeq
    }
    val dtSplittingAttribute: IndexedSeq[Option[Int]] = nodes.toIndexedSeq map { node =>
      node.splittingAttribute
    }
    val dtCategoryCounts: IndexedSeq[Seq[(Int, Int)]] = nodes.toIndexedSeq map { node =>
      (node.categoryCounts ++ priorCounts map {
        case (k, v) =>
          k -> (v + node.categoryCounts.getOrElse(k, 0))
      }).toSeq
    }
    new DecisionTree(dtChild, dtSplittingAttribute, dtCategoryCounts)
  }

  /** Stores a validation vector at this node.
    *
    * @param validationVector the feature vector to store
    */
  def addValidationVector(validationVector: FeatureVector): Unit = {
    require(validationVector.label.isDefined)
    val validationVectorChoice: Int = validationVector.label.get
    validationCounts(validationVectorChoice) += 1
  }

  /** Removes all children from this node. */
  def prune(): Unit = {
    splittingAttribute = None
    childrenMap.clear()
  }

  /** Computes the difference between the number of validation vectors that are correctly
    * classified in the pruned tree versus the unpruned tree.
    */
  def pruningImprovement: Int = numCorrectValidationVectorsIfPruned - numCorrectValidationVectors

  private def numCorrectValidationVectorsIfPruned: Int = {
    validationCounts(choice)
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
object DecisionTreeTrainer {

  /** Factory constructor of DecisionTree.
    *
    * Randomly splits data 2:1 into training:validation.
    * Does reduced-error pruning on validation data.
    * Uses a uniform prior over training labels where each label is assumed to have been
    * seen once already. This Laplace smoothing affects the probability distribution over labels
    * for each instance.
    *
    * @param data training and validation data
    * @return decision tree
    */
  def apply(data: FeatureVectors): DecisionTree = {
    val (trainingSubset, validationSubset) = {
      val n = data.numVectors
      val nTrain = Math.round(n.toFloat * 2 / 3)
      val shuffledInstances = Random.shuffle((0 to n - 1).toIndexedSeq)
      (shuffledInstances.take(nTrain), shuffledInstances.drop(nTrain))
    }
    val root = growTree(data, trainingSubset)
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
    * @return the final decision tree (before any pruning occurs)
    */
  private def growTree(data: FeatureVectors, featureVectorSubset: Seq[Int]): Node = {
    val root = new Node(Some(data), featureVectorSubset,
      (0 to data.numAttributes - 1).toIndexedSeq)
    val stack = mutable.Stack[Node]()
    stack.push(root)
    while (stack.nonEmpty) {
      // nodes on the stack are newly created
      val node = stack.pop()
      // detects termination conditions: if no more attributes are available to split on, or
      // all remaining feature vectors are labeled with the same choice
      if (!(node.attributeSubset.isEmpty ||
        node.featureVectorSubset.forall(data.featureVectors(_).label ==
          data.featureVectors(node.featureVectorSubset.head).label))) {
        val (infoGain, bestAttributeIndex) = maximizeInformationGain(data,
          node.featureVectorSubset, node.attributeSubset)
        if (infoGain > 0) {
          node.setAttribute(node.attributeSubset(bestAttributeIndex))
          val subsets = node.featureVectorSubset groupBy {
            j => data.featureVectors(j).getAttribute(node.attributeSubset(bestAttributeIndex))
          }
          val childAttributeSubset = node.attributeSubset.take(bestAttributeIndex) ++
            node.attributeSubset.drop(bestAttributeIndex + 1)
          for ((featVal, childFeatureVectorSubset) <- subsets) {
            val child = new Node(Some(data), childFeatureVectorSubset, childAttributeSubset)
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
      val inst = data.featureVectors(vectorIndex)
      var cur = root
      cur.addValidationVector(inst)
      while (cur.hasNext(inst)) {
        cur = cur.next(inst)
        cur.addValidationVector(inst)
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

  /** Finds the attribute whose splitting would maximize the information gain.
    *
    * @param data a pointer to a large indexed set of feature vectors
    * @param featureVectorSubset the subset of feature vector indices that interest us
    * @param attributeSubset the subset of attribute indices to consider
    * @return the pair (infoGain, attrIndex), where attributeSubset(attrIndex) is the attribute
    *     whose splitting maximizing the information gain, and where infoGain is the
    *     resulting information gain.
    */
  private def maximizeInformationGain(data: FeatureVectors, featureVectorSubset: Seq[Int],
    attributeSubset: IndexedSeq[Int]): (Double, Int) = {
    require(featureVectorSubset.nonEmpty)
    require(attributeSubset.nonEmpty)
    val unsplitEntropy = computeEntropy(data, featureVectorSubset)
    val informationGainByAttributeValue: IndexedSeq[Double] = for {
      attribute <- attributeSubset
    } yield {
      val vectorsByAttributeValue = featureVectorSubset groupBy {
        data.featureVectors(_).getAttribute(attribute)
      }
      unsplitEntropy - ((vectorsByAttributeValue.values map { x =>
        x.size * computeEntropy(data, x)
      }).sum / featureVectorSubset.size)
    }
    informationGainByAttributeValue.zipWithIndex.max
  }

  /** Computes the entropy of the choice distribution of a set of feature vectors.
    *
    * The two arguments describe a set of FeatureVector objects, each of which is labeled with a
    * choice. The normalized histogram of the choice frequencies is a probability distribution.
    * This function returns the entropy of that distribution.
    *
    * @param data a pointer to a large indexed set of feature vectors
    * @param featureVectorSubset the subset of feature vector indices that interest us
    * @return the entropy of the choice distribution, as described above
    */
  private def computeEntropy(data: FeatureVectors, featureVectorSubset: Seq[Int]) = {
    val choices = featureVectorSubset groupBy { j => data.featureVectors(j).label }
    val frequencyHistogram = choices.values map { l => l.size }
    val unnormalizedEntropy = (frequencyHistogram map { freq =>
      freq.toDouble * math.log(freq)
    }).sum
    val normalizer = frequencyHistogram.sum
    math.log(normalizer) - ((1.0 / normalizer) * unnormalizedEntropy)
  }
}
