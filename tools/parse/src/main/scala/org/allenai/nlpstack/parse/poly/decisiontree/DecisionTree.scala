package org.allenai.nlpstack.parse.poly.decisiontree

import spray.json.DefaultJsonProtocol._
import scala.annotation.tailrec
import scala.collection.mutable

/** Immutable decision tree for integer-valued features and outcomes.
  *
  * Each data structure is an indexed sequence of properties. The ith element of each sequence
  * is the property of node i of the decision tree.
  *
  * @param child stores the children of each node (as a map from attribute values to node ids)
  * @param splittingAttribute stores the attribute that each node splits on; can be None for leaf
  * nodes
  * @param outcomeHistograms for each node, stores a map of outcomes to their frequency of
  * appearance at that node (i.e. how many times a training vector with
  * that outcome makes it to this node during classification)
  */
case class DecisionTree(outcomes: Iterable[Int], child: IndexedSeq[Seq[(Int, Int)]],
  splittingAttribute: IndexedSeq[Option[Int]], outcomeHistograms: IndexedSeq[Seq[(Int, Int)]])
    extends ProbabilisticClassifier {

  @transient lazy val decisionPaths: IndexedSeq[Seq[(Int, Int)]] = {
    var pathMap = Map[Int, Seq[(Int, Int)]]()
    pathMap = pathMap + (0 -> Seq[(Int, Int)]())
    for (decisionTreeNode <- topologicalOrder) {
      val currentPath = pathMap(decisionTreeNode)
      for ((featureValue, decisionTreeChild) <- child(decisionTreeNode)) {
        pathMap = pathMap + (decisionTreeChild ->
          (currentPath :+ Tuple2(splittingAttribute(decisionTreeNode).get, featureValue)))
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

  /** Gets probability distribution of label.
    *
    * @param inst instance to find distribution of
    * @return probability distribution of label according to training data
    */
  override def outcomeDistribution(inst: FeatureVector): Map[Int, Double] = {
    distribution(findDecisionPoint(inst))
  }

  def outcomeHistogram(inst: FeatureVector): Map[Int, Int] = {
    outcomeHistogramMap(findDecisionPoint(inst))
  }

  /** All attributes used in the decision tree. */
  override def allFeatures: Set[Int] = splittingAttribute.flatten.toSet

  /** Map versions of the argument parameters (only included because Spray/JSON was not working
    * with maps for reasons unclear to me).
    */
  @transient private val childMap: IndexedSeq[Map[Int, Int]] = child map { c => c.toMap }
  @transient private val outcomeHistogramMap: IndexedSeq[Map[Int, Int]] = outcomeHistograms map { cc =>
    cc.toMap
  }

  /** The most probable outcome at each node of the decision tree. */
  @transient private val mostProbableOutcome: IndexedSeq[Int] = outcomeHistograms map { cc =>
    (cc maxBy { _._2 })._1
  }

  /** The probability distribution over outcomes for each node of the decision tree.
    *
    * If this tree was trained with [[DecisionTreeTrainer]], then
    * the distribution is Laplacian-smoothed assuming one count for each label
    * in the training data.
    */
  @transient lazy val distribution: IndexedSeq[Map[Int, Double]] = {
    outcomeHistogramMap map { cc =>
      val priorCounts = outcomes.toList.map(_ -> 1).toMap // add-one smoothing
      (ProbabilisticClassifier.normalizeDistribution(
        (ProbabilisticClassifier.addMaps(cc, priorCounts) mapValues { _.toDouble }).toSeq
      )).toMap
    }
  }

  /** From a particular node, chooses the correct child according to the instance
    * and its splitting attribute (if there is one).
    *
    * @param nodeId the id of the node
    * @param inst the instance
    * @return the node id of the correct child (if there is one)
    */
  protected def selectChild(nodeId: Int, inst: FeatureVector): Option[Int] = {
    splittingAttribute(nodeId) flatMap { attr => childMap(nodeId).get(inst.getFeature(attr)) }
  }

  /** Finds the "decision point" of the specified instance. This is the node for which no
    * child covers the instance.
    *
    * @param inst instance to classify
    * @return the node the instance is classified into
    */
  @tailrec protected final def findDecisionPoint(inst: FeatureVector, nodeId: Int = 0): Int = {
    selectChild(nodeId, inst) match {
      case None => nodeId
      case Some(child) => findDecisionPoint(inst, child)
    }
  }

  /** Prints the decision tree to stdout. */
  def print(featureNames: Vector[String], outcomeNames: Vector[String], nodeId: Int = 0,
    tabbing: String = ""): Unit = {

    splittingAttribute(nodeId) match {
      case Some(attr) =>
        println(tabbing + featureNames(attr))
        childMap(nodeId).get(1) match {
          case Some(child) =>
            print(featureNames, outcomeNames, child, tabbing + "  ")
          case None => println(tabbing + "  +")
        }
        childMap(nodeId).get(0) match {
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
  implicit val dtFormat = jsonFormat4(DecisionTree.apply)
}
