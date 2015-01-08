package org.allenai.nlpstack.parse.poly.decisiontree

import scala.collection.mutable
import scala.compat.Platform
import scala.util.Random

/** Internal class of DecisionTreeTrainer.
  *
  * @param data a pointer to a large indexed set of feature vectors
  * @param featureVectorSubset the subset of feature vector indices that interests us
  * @param attributeSubset the subset of attribute indices to consider
  */
private class Node(private var data: Option[FeatureVectors], val featureVectorSubset: Seq[Int],
    val attributeSubset: Seq[Int]) {

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
    //val priorCounts = categories.toList.map(_ -> 1).toMap
    val dtChild: IndexedSeq[Seq[(Int, Int)]] = nodes.toIndexedSeq map { node =>
      (node.childrenMap.toMap mapValues { c => nodeToId(c) }).toSeq
    }
    val dtSplittingAttribute: IndexedSeq[Option[Int]] = nodes.toIndexedSeq map { node =>
      node.splittingAttribute
    }
    val dtCategoryCounts: IndexedSeq[Seq[(Int, Int)]] = nodes.toIndexedSeq map { node =>
      (node.categoryCounts map { //++ priorCounts map {
        case (k, v) =>
          k -> (v + node.categoryCounts.getOrElse(k, 0))
      }).toSeq
    }
    new DecisionTree(categories, dtChild, dtSplittingAttribute, dtCategoryCounts)
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
//param validationPercentage percentage of the training vectors to hold out for validation
class DecisionTreeTrainer(validationPercentage: Double,
  attributesExaminedPerNode: Int = Integer.MAX_VALUE) extends ProbabilisticClassifierTrainer {

  /** Factory constructor of DecisionTree.
    *
    * Randomly splits data into training:validation (according to the validationPercentage param).
    * Does reduced-error pruning on validation data.
    * Uses a uniform prior over training labels where each label is assumed to have been
    * seen once already. This Laplace smoothing affects the probability distribution over labels
    * for each instance.
    *
    * @param data training and validation data
    * @return the induced decision tree
    */
  override def apply(data: FeatureVectors): ProbabilisticClassifier = {

    val (trainingSubset, validationSubset) = {
      val n = data.numVectors
      val nTrain = Math.round(n.toFloat * validationPercentage).toInt
      val shuffledInstances = Random.shuffle((0 to n - 1).toIndexedSeq)
      (shuffledInstances.drop(nTrain), shuffledInstances.take(nTrain))
    }
    println("Growing tree.")
    val root = growTree(data, trainingSubset, attributesExaminedPerNode)
    if (validationSubset.nonEmpty) {
      println("Pruning tree.")
      pruneTree(data, validationSubset, root)
    }
    root.toDecisionTree()
  }

  /** Grows a decision tree from a set of labeled feature vectors, using information gain as
    * the metric by which we split each node.
    *
    * @param data a pointer to a large indexed set of feature vectors
    * @param featureVectorSubset the subset of feature vector indices that interest us
    * @param attributesExaminedPerNode number of random sampled attributes for which we measure
    * information gain at each node
    * @return the final decision tree (before any pruning occurs)
    */
  private def growTree(data: FeatureVectors, featureVectorSubset: Seq[Int],
    attributesExaminedPerNode: Int): Node = {

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

        val (attributesToExamine, attributesToIgnore) = {
          val shuffled = Random.shuffle(node.attributeSubset)
          (shuffled.take(attributesExaminedPerNode), shuffled.drop(attributesExaminedPerNode))
        }

        // can be expensive
        val infoGainByAttr: Seq[(Int, Double)] =
          computeInformationGainUnoptimized(data, node.featureVectorSubset, attributesToExamine) filter {
            case (_, gain) => gain > 0 // we get rid of attributes with zero information gain
          }

        if(infoGainByAttr.nonEmpty) {
          val (bestAttr, _) = infoGainByAttr maxBy { case (_, gain) => gain}
          node.setAttribute(bestAttr)
          val subsets = node.featureVectorSubset groupBy {
            j => data.featureVectors(j).getAttribute(bestAttr)
          }
          val childAttributeSubset = {
            val nonzeroAttrs = infoGainByAttr map { case (attr, _) => attr }
            val bestAttrIndex = nonzeroAttrs.indexOf(bestAttr)
            attributesToIgnore ++ nonzeroAttrs.take(bestAttrIndex) ++
              nonzeroAttrs.drop(bestAttrIndex + 1)
          }
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

  /** Computes the information gain from splitting each attribute.
    *
    * @param data a pointer to a large indexed set of feature vectors
    * @param featureVectorSubset the subset of feature vector indices that interest us
    * @param attributeSubset the subset of attribute indices to consider
    * @return a sequence of pairs (attr, gain), which maps each attribute to its information gain
    */
  private def computeInformationGain(data: FeatureVectors, featureVectorSubset: Seq[Int],
    attributeSubset: Seq[Int]): Seq[(Int, Double)] = {

    require(featureVectorSubset.nonEmpty)
    require(attributeSubset.nonEmpty)
    val featureVectorSubstream: Seq[FeatureVector] = featureVectorSubset map {
      data.featureVectors(_)
    }
    val overallOutcomeHistogram: Map[Int, Int] = (featureVectorSubstream map { featureVector =>
      featureVector.label.get
    }) groupBy { x => x } mapValues { x => x.size }
    val unsplitEntropy = computeEntropy(overallOutcomeHistogram)
    // by far the most expensive step
    val omnibusEnumeration: Seq[(Int, Int, Int)] = (for {
      featureVector <- featureVectorSubstream if featureVector.label != None
      attr <- featureVector.nonzeroAttributes if attributeSubset.contains(attr)
    } yield {
      (attr, featureVector.getAttribute(attr), featureVector.label.get)
    })
    val omnibusHistogram: Map[(Int, Int, Int), Int] = omnibusEnumeration groupBy { x =>
      x } mapValues { x => x.size }
    val attrHistograms: Map[Int, Map[(Int, Int, Int), Int]] =
      omnibusHistogram groupBy { case ((attr, _, _), _) =>
        attr
      }
    val informationGainByAttributeValue: Iterable[Double] = for {
      attribute <- attributeSubset
    } yield {
      val attrValueAndLabelHistogram = attrHistograms.getOrElse(attribute, Map[(Int, Int, Int), Int]())
      val outcomeHistogramsForNonzeroFeatures: Seq[Map[Int, Int]] =
        (attrValueAndLabelHistogram.groupBy {
          case ((_, attrValue, _), _) => attrValue
        } mapValues { labelHist =>
          labelHist map { case ((_, _, vectorLabel), count) =>
            (vectorLabel, count)
          }
        }).values.toSeq
      val outcomeHistogramForZeroFeature: Map[Int, Int] =
        for {
          (outcome, count) <- overallOutcomeHistogram
        } yield {
          (outcome,
            count - (outcomeHistogramsForNonzeroFeatures map { hist => hist.getOrElse(outcome, 0) }).sum)
        }
      val outcomeHistograms = outcomeHistogramForZeroFeature +: outcomeHistogramsForNonzeroFeatures
      val result = unsplitEntropy - ((outcomeHistograms map { histogram =>
        histogram.values.sum * computeEntropy(histogram)
      }).sum / featureVectorSubstream.size)
      result
    }
    attributeSubset.zip(informationGainByAttributeValue)
  }

  private def computeInformationGainUnoptimized(data: FeatureVectors, featureVectorSubset: Seq[Int],
    attributeSubset: Seq[Int]): Seq[(Int, Double)] = {

    require(featureVectorSubset.nonEmpty)
    require(attributeSubset.nonEmpty)

    val featureVectorSubstream: Seq[FeatureVector] = featureVectorSubset map {
      data.featureVectors(_)
    }
    val unsplitEntropy = computeOutcomeEntropy(featureVectorSubstream)
    val informationGainByAttributeValue: Iterable[Double] = for {
      attribute <- attributeSubset
    } yield {
      val vectorsByAttributeValue: Map[Int, Seq[FeatureVector]] =
        featureVectorSubstream groupBy { x => x.getAttribute(attribute) }
      val result = unsplitEntropy - ((vectorsByAttributeValue.values map { x =>
        x.size * computeOutcomeEntropy(x)
      }).sum / featureVectorSubset.size)
      result
    }
    attributeSubset.zip(informationGainByAttributeValue)
  }

  /** Computes the entropy of the outcome distribution of a set of feature vectors.
    *
    * The two arguments describe a set of FeatureVector objects, each of which is labeled with a
    * choice. The normalized histogram of the choice frequencies is a probability distribution.
    * This function returns the entropy of that distribution.
    *
    * param data a pointer to a large indexed set of feature vectors
    * param featureVectorSubset the subset of feature vector indices that interest us
    * @return the entropy of the choice distribution, as described above
    */
  private def computeOutcomeEntropy(featureVectorSubstream: Seq[FeatureVector]) = {
    val outcomeHistogram: Map[Int, Int] =
      featureVectorSubstream groupBy { _.label.get } mapValues { _.size }
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

