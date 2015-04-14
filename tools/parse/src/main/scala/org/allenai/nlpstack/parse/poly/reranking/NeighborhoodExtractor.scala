package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.polyparser.{
  NeighborhoodSource,
  Neighborhood,
  PolytreeParseSource,
  PolytreeParse
}

import reming.DefaultJsonProtocol._

/** Maps a parse tree node to one or more of its neighborhoods.
  *
  * Different extractors will define "neighborhood" in different ways.
  * For instance, one might want to consider neighborhoods like:
  * - a node and its children
  * - a node and its parents
  * - a node and its breadcrumb
  */
trait NeighborhoodExtractor extends ((PolytreeParse, Int) => Seq[Neighborhood])

object NeighborhoodExtractor {
  private implicit val specificParentTransformFormat = jsonFormat1(SpecificParentExtractor.apply)
  private implicit val specificChildTransformFormat = jsonFormat1(SpecificChildExtractor.apply)
  private implicit val selfAndSpecificParentTransformFormat =
    jsonFormat1(SelfAndSpecificParentExtractor.apply)
  private implicit val selfAndSpecificChildTransformFormat =
    jsonFormat1(SelfAndSpecificChildExtractor.apply)
  private implicit val allChildrenExtractorFormat = jsonFormat0(() => AllChildrenExtractor)
  private implicit val allParentsExtractorFormat = jsonFormat0(() => AllParentsExtractor)
  private implicit val eachChildExtractorFormat = jsonFormat0(() => EachChildExtractor)
  private implicit val eachParentExtractorFormat = jsonFormat0(() => EachParentExtractor)
  private implicit val selfExtractorFormat = jsonFormat0(() => SelfExtractor)

  implicit val neighborhoodExtractorJsonFormat = parentFormat[NeighborhoodExtractor](
    childFormat[SpecificParentExtractor, NeighborhoodExtractor],
    childFormat[SpecificChildExtractor, NeighborhoodExtractor],
    childFormat[SelfAndSpecificParentExtractor, NeighborhoodExtractor],
    childFormat[SelfAndSpecificChildExtractor, NeighborhoodExtractor],
    childFormat[AllChildrenExtractor.type, NeighborhoodExtractor],
    childFormat[AllParentsExtractor.type, NeighborhoodExtractor],
    childFormat[EachChildExtractor.type, NeighborhoodExtractor],
    childFormat[EachParentExtractor.type, NeighborhoodExtractor],
    childFormat[SelfExtractor.type, NeighborhoodExtractor]
  )
}

/** Extracts the neighborhood (child1, ..., childK) from a parse tree, where childI is
  * the Ith child of the input token.
  */
case object AllChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(Neighborhood(parse.families(token).tokens.tail))
  }
}

/** Extracts the neighborhood (parent1, ..., parentK) from a parse tree, where parentI is
  * the Ith parent of the input token.
  */
case object AllParentsExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(Neighborhood(parse.getParents().getOrElse(token, Seq())))
  }
}

/** Extracts all neighborhoods of the form (child) from a parse tree, where child is one of the
  * children of the input token.
  */
case object EachChildExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    parse.children(token).toSeq map { child => Neighborhood(Seq(child)) }
  }
}

/** Extracts all neighborhoods of the form (parent) from a parse tree, where parent is one of the
  * parents of the input token.
  */
case object EachParentExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    parse.getParents().getOrElse(token, Seq()) map { parent => Neighborhood(Seq(parent)) }
  }
}

/** Extracts neighborhood (child_k), where child_k is the kth child of the input token.
  *
  * If the input token does not have a kth child, the apply operation
  * will return the empty sequence.
  */
case class SpecificChildExtractor(k: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(
      parse.families(token).tokens.lift(k + 1) map { x => Neighborhood(Seq(x)) }
    ).flatten
  }
}

/** Extracts neighborhood (parent_k), where parent_k is the kth parent of the input token.
  *
  * If the input token does not have a kth parent, the apply operation
  * will return the empty sequence.
  */
case class SpecificParentExtractor(parentIndex: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(
      parse.getParents().getOrElse(token, Seq[Int]()).lift(parentIndex) map { x =>
        Neighborhood(Seq(x))
      }
    ).flatten
  }
}

/** Extracts neighborhood (child_k, token), where child_k is the kth child of the input token.
  *
  * If the input token does not have a kth child, the apply operation
  * will return the empty sequence.
  */
case class SelfAndSpecificChildExtractor(k: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(
      parse.families(token).tokens.lift(k + 1) map { x => Neighborhood(Seq(x, token)) }
    ).flatten
  }
}

/** Extracts neighborhood (parent_k, token), where parent_k is the kth parent of the input token.
  *
  * If the input token does not have a kth parent, the apply operation
  * will return the empty sequence.
  */
case class SelfAndSpecificParentExtractor(parentIndex: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(
      parse.getParents().getOrElse(token, Seq[Int]()).lift(parentIndex) map { x =>
        Neighborhood(Seq(x, token))
      }
    ).flatten
  }
}

/** Extracts neighborhood (token) from the parse tree. */
case object SelfExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(Neighborhood(Seq(token)))
  }
}

/** Applies a NeighborhoodExtractor to all tokens in a parse tree and returns an iterator
  * over all of the extracted neighborhoods.
  *
  * @param subextractor the NeighborhoodExtractor to apply to all tokens
  */
class ParseNeighborhoodExtractor(subextractor: NeighborhoodExtractor) {

  def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    Range(0, parse.tokens.size).iterator flatMap { tokenIndex =>
      subextractor(parse, tokenIndex)
    }
  }
}

/** Applies a ParseNeighborhoodExtractor to all parses in a PolytreeParseSource and returns
  * an iterator over all of the extracted neighborhoods.
  *
  * @param parseSource the source of parses
  * @param extractor the extractor for extracting neighborhoods from each parse
  */
class ExtractorBasedNeighborhoodSource(
    parseSource: PolytreeParseSource,
    extractor: NeighborhoodExtractor
) extends NeighborhoodSource {

  private val parseExtractor = new ParseNeighborhoodExtractor(extractor)

  override def getNeighborhoodIterator(): Iterator[(PolytreeParse, Neighborhood)] = {
    parseSource.parseIterator flatMap { parse =>
      parseExtractor(parse) map { neighborhood =>
        (parse, neighborhood)
      }
    }
  }
}
