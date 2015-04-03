package org.allenai.nlpstack.parse.poly.polyparser

/** The ArcInverter takes a PolytreeParse and inverts arcs whose labels are in the argument set
  * `inverseArcLabels`. Note that this operation should only affect the `children` field of a
  * PolytreeParse, since the other fields only care about the underlying undirected tree.
  *
  * The purpose of this class is to convert standard dependency parses into polytree
  * dependency parses. For instance, we may wish to invert all arcs x ---> y for which
  * the arc label is 'det (effectively this would invert the relationship between a determiner
  * and its noun to say that the determiner "requires" the noun, rather than vice-versa).
  *
  * @param inverseArcLabels the set of arc labels to invert
  */
class ArcInverter(val inverseArcLabels: Set[ArcLabel]) extends (PolytreeParse => PolytreeParse) {

  /** Inverts the arcs whose labels are in `inverseArcLabels`
    *
    * @param parse the polytree parse we want to transform
    * @return a new polytree parse, with the specified arcs inverted
    */
  def apply(parse: PolytreeParse): PolytreeParse = {

    // for each node, determine the neighbors for which the arcs should be inverted
    val invertibleNeighbors: Vector[Set[Int]] = for {
      labeledNeighbors <- parse.arclabels
    } yield for {
      (neighbor, label) <- labeledNeighbors if isInvertible(label)
    } yield neighbor

    // compute the new children using an XOR operation
    val newChildren: Vector[Set[Int]] = for {
      (neighbors, children) <- invertibleNeighbors.zip(parse.children)
    } yield ((neighbors diff children) union (children diff neighbors))

    PolytreeParse(parse.sentence, parse.breadcrumb, newChildren, parse.arclabels)
  }

  def isInvertible(arcLabel: ArcLabel): Boolean = {
    val stanLabel = arcLabel match {
      case dpLabel: DependencyParsingArcLabel =>
        dpLabel.stanLabel
      case _ =>
        arcLabel.toSymbol
    }
    inverseArcLabels.contains(SingleSymbolArcLabel(stanLabel))
  }

}
