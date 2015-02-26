package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.polyparser.{ PolytreeParseSource, PolytreeParse }
import spray.json.DefaultJsonProtocol._
import spray.json._

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

  /** Boilerplate code to serialize a NeighborhoodExtractor to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM NeighborhoodExtractor, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object NeighborhoodExtractorJsonFormat extends RootJsonFormat[NeighborhoodExtractor] {

    implicit val specificParentTransformFormat =
      jsonFormat1(SpecificParentExtractor.apply).pack("type" -> "SpecificParentExtractor")
    implicit val specificChildTransformFormat =
      jsonFormat1(SpecificChildExtractor.apply).pack("type" -> "SpecificChildExtractor")
    implicit val selfAndSpecificParentTransformFormat =
      jsonFormat1(SelfAndSpecificParentExtractor.apply).pack(
        "type" -> "SelfAndSpecificParentExtractor"
      )
    implicit val selfAndSpecificChildTransformFormat =
      jsonFormat1(SelfAndSpecificChildExtractor.apply).pack("type" ->
        "SelfAndSpecificChildExtractor")

    def write(extractor: NeighborhoodExtractor): JsValue = extractor match {
      case specificParentExtractor: SpecificParentExtractor => specificParentExtractor.toJson
      case specificChildExtractor: SpecificChildExtractor => specificChildExtractor.toJson
      case selfAndSpecificParentExtractor: SelfAndSpecificParentExtractor =>
        selfAndSpecificParentExtractor.toJson
      case selfAndSpecificChildExtractor: SelfAndSpecificChildExtractor =>
        selfAndSpecificChildExtractor.toJson
      case AllChildrenExtractor => JsString("AllChildrenExtractor")
      case AllParentsExtractor => JsString("AllParentsExtractor")
      case EachChildExtractor => JsString("EachChildExtractor")
      case EachParentExtractor => JsString("EachParentExtractor")
      case SelfExtractor => JsString("SelfExtractor")
    }

    def read(value: JsValue): NeighborhoodExtractor = value match {
      case JsString(typeid) => typeid match {
        case "AllChildrenExtractor" => AllChildrenExtractor
        case "AllParentsExtractor" => AllParentsExtractor
        case "EachChildExtractor" => EachChildExtractor
        case "EachParentExtractor" => EachParentExtractor
        case "SelfExtractor" => SelfExtractor
        case x => deserializationError(s"Invalid identifier for NeighborhoodExtractor: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        specificParentTransformFormat,
        specificChildTransformFormat,
        selfAndSpecificParentTransformFormat,
        selfAndSpecificChildTransformFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** Extracts the neighborhood (child1, ..., childK) from a parse tree, where childI is
  * the Ith child of the input token.
  */
case object AllChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse, token: Int): Seq[Neighborhood] = {
    Seq(Neighborhood(parse.families(token).tail))
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
      parse.families(token).lift(k + 1) map { x => Neighborhood(Seq(x)) }
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
      parse.families(token).lift(k + 1) map { x => Neighborhood(Seq(x, token)) }
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
