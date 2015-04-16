package org.allenai.nlpstack.parse.poly.polyparser

import reming.DefaultJsonProtocol._

/** A Neighborhood is a sequence of token indices, generally referring to a parse tree.
  *
  * For instance, one might want to consider neighborhoods like:
  * - a node and its children
  * - a node and its parents
  * - a node and its breadcrumb
  *
  * @param tokens a sequence of token indices, usually associated in some way
  * (see NeighborhoodExtractor instances for examples of such associations)
  */
case class Neighborhood(tokens: Seq[Int])

object Neighborhood {
  implicit val neighborhoodJsonFormat = jsonFormat1(Neighborhood.apply)
}

/** A data source for neighborhoods. */
trait NeighborhoodSource {
  /** Returns an iterator over the neighborhoods in this data source.
    *
    * @return an iterator over the neighborhoods in this data source
    */
  def getNeighborhoodIterator(): Iterator[(PolytreeParse, Neighborhood)]
}
