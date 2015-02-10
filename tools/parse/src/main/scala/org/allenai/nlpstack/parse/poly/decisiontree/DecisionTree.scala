package org.allenai.nlpstack.parse.poly.decisiontree

import spray.json._
import scala.annotation.tailrec
import scala.collection.mutable

/** Immutable decision tree for integer-valued features and outcomes.
  *
  * Each data structure is an indexed sequence of properties. The ith element of each sequence
  * is the property of node i of the decision tree.
  *
  * @param outcomes all possible outcomes for the decision tree
  * @param child stores the children of each node (as a map from feature values to node ids)
  * @param splittingFeature stores the feature that each node splits on; can be None for leaf
  * nodes
  * @param outcomeHistograms for each node, stores a map of outcomes to their frequency of
  * appearance at that node (i.e. how many times a training vector with
  * that outcome makes it to this node during classification)
  */
case class DecisionTree(outcomes: Iterable[Int], child: IndexedSeq[Map[Int, Int]],
  splittingFeature: IndexedSeq[Option[Int]], outcomeHistograms: IndexedSeq[Map[Int, Int]])
    extends ProbabilisticClassifier {

  /*
  @transient lazy val decisionPaths: IndexedSeq[Seq[(Int, Int)]] = {
    var pathMap = Map[Int, Seq[(Int, Int)]]()
    pathMap = pathMap + (0 -> Seq[(Int, Int)]())
    for (decisionTreeNode <- topologicalOrder) {
      val currentPath = pathMap(decisionTreeNode)
      for ((featureValue, decisionTreeChild) <- child(decisionTreeNode)) {
        pathMap = pathMap + (decisionTreeChild ->
          (currentPath :+ Tuple2(splittingFeature(decisionTreeNode).get, featureValue)))
      }
    }
    Range(0, child.size) map { node => pathMap(node) }
  }


  @transient lazy val topologicalOrder: Seq[Int] = {
    val stack = mutable.Stack[Int]()
    var topoList = Seq[Int]()
    stack.push(0)
    while (stack.nonEmpty) {
      val node = stack.pop()
      topoList = node +: topoList
      for ((_, childId) <- child(node)) {
        stack.push(childId)
      }
    }
    topoList.reverse
  }
  */

  /** Gets a probability distribution over possible outcomes..
    *
    * @param featureVector feature vector to compute the distribution for
    * @return probability distribution of outcomes according to training data
    */
  override def outcomeDistribution(featureVector: FeatureVector): Map[Int, Double] = {
    distribution(findDecisionPoint(featureVector))
  }

  def outcomeHistogram(featureVector: FeatureVector): Map[Int, Int] = {
    outcomeHistograms(findDecisionPoint(featureVector))
  }

  /** All features used in the decision tree. */
  override def allFeatures: Set[Int] = splittingFeature.flatten.toSet

  /** The most probable outcome at each node of the decision tree. */
  @transient private lazy val mostProbableOutcome: IndexedSeq[Int] = outcomeHistograms map { cc =>
    (cc maxBy { _._2 })._1
  }

  /** The probability distribution over outcomes for each node of the decision tree.
    *
    * If this tree was trained with [[DecisionTreeTrainer]], then
    * the distribution is Laplacian-smoothed assuming one count for each label
    * in the training data.
    */
  @transient lazy val distribution: IndexedSeq[Map[Int, Double]] = {
    outcomeHistograms map { cc =>
      val priorCounts = outcomes.toList.map(_ -> 1).toMap // add-one smoothing
      (ProbabilisticClassifier.normalizeDistribution(
        (ProbabilisticClassifier.addMaps(cc, priorCounts) mapValues { _.toDouble }).toSeq
      )).toMap
    }
  }

  /** From a particular node, chooses the correct child according to the feature vector
    * and the node's splitting feature (if there is one).
    *
    * @param nodeId the id of the node
    * @param featureVector the feature vector
    * @return the node id of the correct child (if there is one)
    */
  protected def selectChild(nodeId: Int, featureVector: FeatureVector): Option[Int] = {
    splittingFeature(nodeId) flatMap { feature =>
      child(nodeId).get(featureVector.getFeature(feature))
    }
  }

  /** Finds the "decision point" of the specified feature vector. This is the node for which no
    * child covers the feature vector.
    *
    * @param featureVector feature vector to classify
    * @return the decision tree node that the feature vector is classified into
    */
  @tailrec protected final def findDecisionPoint(
    featureVector: FeatureVector, nodeId: Int = 0
  ): Int = {
    selectChild(nodeId, featureVector) match {
      case None => nodeId
      case Some(child) => findDecisionPoint(featureVector, child)
    }
  }

  /** Prints the decision tree to stdout. */
  def print(featureNames: Vector[String], outcomeNames: Vector[String], nodeId: Int = 0,
    tabbing: String = ""): Unit = {

    splittingFeature(nodeId) match {
      case Some(feature) =>
        println(tabbing + featureNames(feature))
        child(nodeId).get(1) match {
          case Some(child) =>
            print(featureNames, outcomeNames, child, tabbing + "  ")
          case None => println(tabbing + "  +")
        }
        child(nodeId).get(0) match {
          case Some(child) =>
            print(featureNames, outcomeNames, child, tabbing + "  ")
          case None => println(tabbing + "  -")
        }
      case None =>
        println(tabbing + outcomeNames(mostProbableOutcome(nodeId)))
    }
  }
}

object DecisionTree {
  import spray.json.DefaultJsonProtocol._
  implicit val dtFormat = jsonFormat4(DecisionTree.apply)
}
