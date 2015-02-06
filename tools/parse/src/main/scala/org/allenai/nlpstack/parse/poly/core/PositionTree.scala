package org.allenai.nlpstack.parse.poly.core

import scala.math.Ordering.Implicits._

/** A Position is a essentially a pointer to a node in a rooted, directed tree. It is a
  * sequence of non-negative integers.
  *
  * It is probably easiest to describe the concept by example.
  * The root of the tree is the empty sequence. The first child of the root is Seq(0) (we count
  * from zero). The third child of the first child of the root is Seq(2, 0). The seventh child
  * of the third child of the first child of the root is Seq(6, 2, 0). Etc.
  *
  * @param components the sequence of integers corresponding to a node's position in a tree
  */
case class Position(components: Seq[Int]) {

  /** The depth of the position in the tree.
    *
    * i.e. the depth of the root is zero, the depth of the root's children is one, etc.
    */
  @transient lazy val depth = components.size

  /** Gets the kth child of this position.
    *
    * @param childIndex the number of the desired child (counting from zero)
    * @return the child's position
    */
  def getChild(childIndex: Int): Position = {
    require(childIndex >= 0)
    Position(childIndex +: components)
  }

  /** Gets the parent of this position (if it exists).
    *
    * This returns None if this is the root position.
    */
  @transient lazy val parent: Option[Position] = {
    if (components.isEmpty) {
      None
    } else {
      Some(Position(components.tail))
    }
  }

  /** Returns whether the specified position is a descendant of this position.
    *
    * This returns true if the argument position is the same as this position (i.e. a position
    * is considered by this function to be its own descendant).
    *
    * @param position the position of interest
    * @return whether the specified position is a descendant of this position
    */
  def isDescendentOf(position: Position): Boolean = {
    components.endsWith(position.components)
  }

  override def toString(): String = s"[${components.reverse.mkString(" ")}]"
}

object Position {
  /** The root position (i.e. the empty sequence). */
  val root = Position(Seq.empty)

  /** Sorts the specified positions according to a depth-first preorder search.
    *
    * @param positions the positions we want to sort
    * @return the same positions, sorted according to a depth-first preorder search
    */
  def depthFirstPreorder(positions: Seq[Position]): Seq[Position] = {
    positions.sortBy { pos => pos.components.reverse }
  }
}

/** A node of a position tree.
  *
  * @param labels a map from label names to label values
  */
case class PositionTreeNode(labels: Map[Symbol, String])

/** A PositionTree is a rooted, directed tree, implemented as a map from positions to nodes.
  *
  * @param nodes a sequence of (position, node) pairs
  */
case class PositionTree(nodes: Seq[(Position, PositionTreeNode)]) {

  /** The positions in the tree. */
  lazy val positions: Seq[Position] = nodes map { _._1 }

  /** A map from positions to nodes. */
  lazy val positionMap = nodes.toMap

  /** Gets the child positions that appear in this tree.
    *
    * @param position the position of interest
    * @return the child positions that appear in this tree
    */
  def getChildren(position: Position): Seq[Position] = {
    positions filter { childPosition =>
      childPosition.parent match {
        case Some(parentPos) =>
          parentPos == position
        case None => false
      }
    }
  }

  /** Gets the value of the specified label for the node at the specified position.
    *
    * @param position the position of interest
    * @param labelName the label of interest
    * @return the value of the specified label
    */
  def getLabel(position: Position, labelName: Symbol): Option[String] = {
    positionMap.get(position) flatMap { node =>
      node.labels.get(labelName)
    }
  }

  /** The leaf positions of the tree, sorted in depth-first left-to-right order. */
  lazy val leaves: Seq[Position] = Position.depthFirstPreorder(
    positions filter { pos => !positions.contains(pos.getChild(0)) }
  )

  /** Swaps the subtrees at two different positions.
    *
    * This requires that the specified positions are both valid, and are
    * not descendants of one another.
    *
    * @param position1 the first position
    * @param position2 the second position
    * @return a new PositionTree, in which the two specified subtrees have been swapped
    */
  def swapPositions(position1: Position, position2: Position): PositionTree = {
    require(positions.contains(position1), s"position ${position1} is not a valid position")
    require(positions.contains(position2), s"position ${position2} is not a valid position")
    require(
      !position1.isDescendentOf(position2) && !position2.isDescendentOf(position1),
      s"swapped positions cannot be descendants of each other: ${position1} and ${position2}"
    )
    PositionTree(
      nodes map {
        case (position, node) =>
          val transformedPosition =
            if (position.isDescendentOf(position1)) {
              Position(position.components.take(position.depth - position1.depth)
                ++ position2.components)
            } else if (position.isDescendentOf(position2)) {
              Position(position.components.take(position.depth - position2.depth)
                ++ position1.components)
            } else {
              position
            }
          (transformedPosition, node)
      }
    )
  }

  /** Substitutes the subtree at the specified position with a new subtree.
    *
    * @param position the desired substitution point
    * @param positionTree the new subtree
    * @return a new PositionTree, in which the subtree at the specified position has been replaced
    */
  def substitute(position: Position, positionTree: PositionTree): PositionTree = {
    require(positions.contains(position), s"Cannot substitute at invalid position ${position}")
    val retained = nodes filter {
      case (pos, node) =>
        !pos.isDescendentOf(position)
    }
    val substituted = positionTree.nodes map {
      case (pos, node) =>
        (Position(pos.components ++ position.components), node)
    }
    PositionTree(retained ++ substituted)
  }
}

class SubstitutionNode(label: String) extends PositionTreeNode(
  Map(ConstituencyParse.wordLabelName -> label)
)

class SubstitutionTree(label: String) extends PositionTree(
  Seq((Position.root, new SubstitutionNode(label)))
)

object ConstituencyParse {
  val constituencyLabelName = 'constituent
  val headLabel = "head"
  val nexusLabel = "nexus"
  val wordLabelName = 'word

  /** Retrieves the words of the constituency parse.
    *
    * @param positionTree a position tree corresponding to a constituency parse
    * @return the words of the tree, in left-to-right order
    */
  def getWords(positionTree: PositionTree): Seq[String] = {
    positionTree.leaves flatMap { leaf =>
      positionTree.getLabel(leaf, ConstituencyParse.wordLabelName)
    }
  }
}
