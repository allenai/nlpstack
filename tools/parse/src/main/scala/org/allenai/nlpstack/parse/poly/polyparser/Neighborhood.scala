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

/** Maps a parse tree to an iterator over its neighborhoods.
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
trait NeighborhoodExtractor extends (PolytreeParse => Iterator[Neighborhood])

object NeighborhoodExtractor {
  def preprocessParse(parse: PolytreeParse): (PolytreeParse, Seq[Token]) = {
    //val mappedParse = PolytreeParse.arcInverterStanford(parse)
    val mappedTokens = parse.tokens.zipWithIndex map { case (tok, index) =>
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

    implicit val rootPathExtractorFormat =
      jsonFormat1(RootPathExtractor.apply).pack("type" -> "RootPathExtractor")

    def write(extractor: NeighborhoodExtractor): JsValue = extractor match {
      case ChildrenExtractor => JsString("ChildrenExtractor")
      case LeftChildrenExtractor => JsString("LeftChildrenExtractor")
      case RightChildrenExtractor => JsString("RightChildrenExtractor")
      case BreadcrumbExtractor => JsString("BreadcrumbExtractor")
      case ParentExtractor => JsString("ParentExtractor")
      case rootPathExtractor: RootPathExtractor =>
        rootPathExtractor.toJson
    }

    def read(value: JsValue): NeighborhoodExtractor = value match {
      case JsString(typeid) => typeid match {
        case "ChildrenExtractor" => ChildrenExtractor
        case "LeftChildrenExtractor" => LeftChildrenExtractor
        case "RightChildrenExtractor" => RightChildrenExtractor
        case "BreadcrumbExtractor" => BreadcrumbExtractor
        case "ParentExtractor" => ParentExtractor
        case x => deserializationError(s"Invalid identifier for NeighborhoodExtractor: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(rootPathExtractorFormat)
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** Extracts neighborhoods of the form (node, child1, ..., childN) from a parse tree. */
case object ChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    (mappedParse.families map { family =>
      Neighborhood(family map { tok =>
        mappedTokens(tok)
      })
    }).iterator
  }
}

/** Extracts neighborhoods of the form (node, leftChild1, ..., leftChildN) from a parse tree. */
case object LeftChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    (mappedParse.families map { family =>
      val onlyLeftChildren = family filter { child => child <= family.head }
      Neighborhood(onlyLeftChildren map { tok =>
        mappedTokens(tok)
      })
    }).iterator
  }
}

/** Extracts neighborhoods of the form (node, rightChild1, ..., rightChildN) from a parse tree. */
case object RightChildrenExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    (mappedParse.families map { family =>
      val onlyRightChildren = family filter { child => child >= family.head }
      Neighborhood(onlyRightChildren map { tok =>
        mappedTokens(tok)
      })
    }).iterator
  }
}

/** Extracts neighborhoods of the form (node, breadcrumb) from a parse tree. */
case object BreadcrumbExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    (mappedParse.breadcrumb.zipWithIndex.tail map { case (crumb, node) =>
      Neighborhood(Seq(node, crumb) map { tok =>
        mappedTokens(tok)
      })
    }).iterator
  }
}

/** Extracts neighborhoods of the form (node, breadcrumb, grandcrumb, ..., root)
  * from a parse tree.
  */
case class RootPathExtractor(maxPathLength: Int) extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    val (_, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    (parse.paths.zipWithIndex.tail map { case (path, node) =>
      Neighborhood((path :+ node).takeRight(maxPathLength) map { tok =>
        mappedTokens(tok)
      })
    }).iterator
  }
}

/** Extracts neighborhoods of the form (node, parent1, ..., parentN) from a parse tree. */
case object ParentExtractor extends NeighborhoodExtractor {

  override def apply(parse: PolytreeParse): Iterator[Neighborhood] = {
    val (mappedParse, mappedTokens) = NeighborhoodExtractor.preprocessParse(parse)
    val parentMap: Map[Int, Seq[Int]] = mappedParse.getParents()
    (Range(0, mappedTokens.size) map { node =>
      Neighborhood(
        (node +: parentMap.getOrElse(node, Seq[Int]())) map { index =>
          mappedTokens(index)
        })
    }).iterator
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
class ExtractorBasedNeighborhoodSource(parseSource: PolytreeParseSource,
  extractor: NeighborhoodExtractor) extends NeighborhoodSource {

  override def getNeighborhoodIterator(): Iterator[Neighborhood] = {
    parseSource.parseIterator flatMap { parse =>
      extractor(parse)
    }
  }
}
