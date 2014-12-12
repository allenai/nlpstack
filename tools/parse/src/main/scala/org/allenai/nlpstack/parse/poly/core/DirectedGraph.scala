package org.allenai.nlpstack.parse.poly.core

/** A node of a directed graph.
  *
  * @param labels a labeling of this node
  */
case class DirectedGraphNode(labels: Map[Symbol, String])

/** An edge of a directed graph.
  *
  * @param from index of the node at the edge's tail
  * @param to index of the node at the edge's head
  * @param labels a labeling of this edge
  */
case class DirectedGraphEdge(from: Int, to: Int, labels: Map[Symbol, String])

/** A directed graph.
  *
  * The index of a node is its position in the `nodes` list (counting from 0).
  *
  * @param nodes the nodes of the directed graph
  * @param edgesByNode each node's outgoing edges
  */
case class DirectedGraph(nodes: IndexedSeq[DirectedGraphNode],
  edgesByNode: IndexedSeq[Seq[DirectedGraphEdge]]) {

  require(nodes.size == edgesByNode.size, "arguments to DirectedGraph must be sequences of" +
    " equivalent length")

  @transient val edges: Set[DirectedGraphEdge] = edgesByNode.flatten.toSet

  /** The sinks of the graph (i.e. nodes with no outgoing edges). */
  @transient val sinks: Set[Int] = {
    (Range(0, nodes.size) filter { node =>
      getOutgoingEdges(node).isEmpty
    }).toSet
  }

  /** Gets the outgoing edges for the specified node.
    *
    * @param nodeIndex the node of interest
    * @return the outgoing edges for the specified node
    */
  def getOutgoingEdges(nodeIndex: Int): Seq[DirectedGraphEdge] = {
    require(nodeIndex >= 0 && nodeIndex < edgesByNode.size, s"node index ${nodeIndex} is out of" +
      " range for this graph")
    edgesByNode(nodeIndex)
  }

  /** Converts this directed graph into a position tree.
    *
    * Note: this is a recursive function. It will throw an exception if the graph is cyclic.
    *
    * @param rootNode index corresponding to the node you want to be the root of the tree
    * @return this graph, as a position tree
    */
  def toPositionTree(rootNode: Int): PositionTree = PositionTree(toPositionTreeHelper(rootNode))

  private def toPositionTreeHelper(rootNode: Int,
    positionSoFar: Position = Position.root,
    encounteredNodes: Set[Int] = Set.empty): Seq[(Position, PositionTreeNode)] = {

    require(!encounteredNodes.contains(rootNode), "cannot convert a cyclic graph to a tree")
    val positionTreeNode = PositionTreeNode(nodes(rootNode).labels)
    val descendantPositions = getOutgoingEdges(rootNode).zipWithIndex flatMap { case (edge, edgeIndex) =>
      toPositionTreeHelper(edge.to, positionSoFar.getChild(edgeIndex), encounteredNodes + rootNode)
    }
    (positionSoFar, positionTreeNode) +: descendantPositions
  }
}
