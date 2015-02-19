package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.Token
import org.allenai.common.json._
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A Neighborhood is a sequence of tokens, generally taken from a parse tree.
  *
  * For instance, one might want to consider neighborhoods like:
  * - a node and its children
  * - a node and its parents
  * - a node and its breadcrumb
  *
  * @param tokens a sequence of tokens, usually associated in some way (see NeighborhoodExtractors
  * for examples of such associations)
  */
case class Neighborhood(tokens: Seq[Token])

object Neighborhood {
  implicit val neighborhoodJsonFormat = jsonFormat1(Neighborhood.apply)

  /** Tallies the number of neighborhood occurrences in a stream of neighborhoods.
    *
    * @param neighborhoodSource the input source
    * @return a histogram associating each neighborhood with its frequency in the input source
    */
  def countNeighborhoods(neighborhoodSource: NeighborhoodSource): Seq[(Neighborhood, Int)] = {
    var counts = Map[Neighborhood, Int]()
    for (event <- neighborhoodSource.getNeighborhoodIterator()) {
      counts = counts + (event -> (1 + counts.getOrElse(event, 0)))
    }
    counts.toSeq
  }
}

/** Maps a parse tree node to one of its neighborhoods.
  *
  * Different extractors will define "neighborhood" in different ways.
  * For instance, one might want to consider neighborhoods like:
  * - a node and its children
  * - a node and its parents
  * - a node and its breadcrumb
  *
  * TODO: create unit tests for all inheriting instances.
  *
  */
trait NeighborhoodExtractor extends ((PolytreeParse, Int) => Seq[Neighborhood])

object NeighborhoodExtractor {
  def preprocessParse(parse: PolytreeParse): (PolytreeParse, Seq[Token]) = {
    val mappedTokens = parse.tokens.zipWithIndex map {
      case (tok, index) =>
        tok.updateProperties(Map('arclabel -> Set(parse.breadcrumbArcLabel(index))))
    }
    (parse, mappedTokens)
  }

  /** Boilerplate code to serialize a NeighborhoodExtractor to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM NeighborhoodExtractor, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object NeighborhoodExtractorJsonFormat extends RootJsonFormat[NeighborhoodExtractor] {

    implicit val singleParentTransformFormat =
      jsonFormat1(SingleParentExtractor.apply).pack("type" -> "SingleParentExtractor")
    implicit val singleChildTransformFormat =
      jsonFormat1(SingleChildExtractor.apply).pack("type" -> "SingleChildExtractor")

    def write(extractor: NeighborhoodExtractor): JsValue = extractor match {
      case singleParentExtractor: SingleParentExtractor => singleParentExtractor.toJson
      case singleChildExtractor: SingleChildExtractor => singleChildExtractor.toJson
      case ChildrenExtractor => JsString("ChildrenExtractor")
      case SelfExtractor => JsString("SelfExtractor")
    }

    def read(value: JsValue): NeighborhoodExtractor = value match {
      case JsString(typeid) => typeid match {
        case "ChildrenExtractor" => ChildrenExtractor
        case "SelfExtractor" => SelfExtractor
        case x => deserializationError(s"Invalid identifier for NeighborhoodExtractor: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        singleParentTransformFormat,
        singleChildTransformFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** Extracts neighborhoods of the form (node, child1, ..., childN) from a parse tree. */
case object ChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(parse.families(token) map { tok => parse.tokens(tok) }))
  }
}

/** Extracts neighborhoods of the form (node, lchild1, ..., lchildN) from a parse tree. */
case object LeftChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(parse.families(token) filter { tok => tok < token } map { tok => parse.tokens(tok) }))
  }
}

/** Extracts neighborhoods of the form (node, rchild1, ..., rchildN) from a parse tree. */
case object RightChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(parse.families(token) filter { tok => tok > token } map { tok => parse.tokens(tok) }))
  }
}

/** Extracts neighborhoods of the form (childK) from a parse tree. */
case class SingleChildExtractor(childIndex: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(Seq(parse.families(token).lift(childIndex)).flatten map { tok => parse.tokens(tok) }))
  }
}

/** Extracts neighborhoods of the form (childK) from a parse tree. */
case class SingleParentExtractor(parentIndex: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(Seq(parse.getParents().getOrElse(token, Seq[Int]()).lift(parentIndex)).flatten map { tok => parse.tokens(tok) }))
  }
}

/** Extracts neighborhoods of the form (childK) from a parse tree. */
case class SingleLeftChildExtractor(childIndex: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(Seq((parse.families(token) filter { tok => tok < token }).lift(childIndex)).flatten map { tok => parse.tokens(tok) }))
  }
}

/** Extracts neighborhoods of the form (childK) from a parse tree. */
case class SingleRightChildExtractor(childIndex: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(Seq((parse.families(token) filter { tok => tok > token }).lift(childIndex)).flatten map { tok => parse.tokens(tok) }))
  }
}

/** Extracts neighborhoods of the form (parent, child) from a parse tree. */
case object ParentChildExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    parse.children(token).toSeq map { child =>
      Neighborhood(Seq(parse.tokens(token), parse.tokens(child)))
    }
  }
}

/** Extracts neighborhoods of the form (node) from a parse tree. */
case object SelfExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    //val (_, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    Seq(Neighborhood(Seq(parse.tokens(token))))
  }
}

/** Maps a parse tree an iterator over its neighborhoods.
  *
  * Different extractors will define "neighborhood" in different ways.
  * For instance, one might want to consider neighborhoods like:
  * - a node and its children
  * - a node and its parents
  * - a node and its breadcrumb
  *
  */
class ParseNeighborhoodExtractor(subextractor: NeighborhoodExtractor) {

  def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    Range(0, parse.tokens.size).iterator flatMap { tokenIndex =>
      subextractor(parse, tokenIndex)
    }
  }
}

/** A data source for neighborhoods. */
trait NeighborhoodSource {
  /** Returns an iterator over the neighborhoods in this data source.
    *
    * @return an iterator over the neighborhoods in this data source
    */
  def getNeighborhoodIterator(): Iterator[Neighborhood]
}

/** Iterates through all neighborhoods from all parses in a PolytreeParseSource.
  *
  * @param parseSource the source of parses
  * @param extractor the extractor for extracting neighborhoods from each parse
  */
class ExtractorBasedNeighborhoodSource(
    parseSource: PolytreeParseSource,
    extractor: ParseNeighborhoodExtractor
) extends NeighborhoodSource {

  override def getNeighborhoodIterator(): Iterator[Neighborhood] = {
    parseSource.parseIterator flatMap { parse =>
      extractor(parse)
    }
  }
}
