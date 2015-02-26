package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.ml.FeatureName
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A NeighborhoodTransform maps a Neighborhood to zero or more feature names.
  *
  * An example might help. Suppose that we have a neighborhood consisting of
  * (node, child1, child2), i.e. three nodes of a parse tree. A transform might map these
  * to the sequence of their POS tags, e.g. FeatureName(Seq('VERB, 'NOUN, 'NOUN)).
  */
trait NeighborhoodTransform extends ((PolytreeParse, Neighborhood) => Seq[FeatureName])

object NeighborhoodTransform {

  /** Boilerplate code to serialize an EventTransform to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM EventTransform, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object NeighborhoodTransformJsonFormat extends RootJsonFormat[NeighborhoodTransform] {

    implicit val propertyNhTransformFormat =
      jsonFormat1(PropertyNhTransform.apply).pack("type" -> "PropertyNhTransform")
    implicit val suffixNeighborhoodTransformFormat =
      jsonFormat1(SuffixNhTransform.apply).pack("type" -> "SuffixNeighborhoodTransform")
    implicit val keywordNeighborhoodTransformFormat =
      jsonFormat1(KeywordNhTransform.apply).pack("type" -> "KeywordNeighborhoodTransform")

    def write(feature: NeighborhoodTransform): JsValue = feature match {
      case ArclabelNhTransform => JsString("ArcLabelNeighborhoodTransform")
      case DirectionNhTransform => JsString("DirectionNeighborhoodTransform")
      case CardinalityNhTransform => JsString("CardinalityNhTransform")
      case propertyNhTransform: PropertyNhTransform =>
        propertyNhTransform.toJson
      case suffixNeighborhoodTransform: SuffixNhTransform =>
        suffixNeighborhoodTransform.toJson
      case keywordNeighborhoodTransform: KeywordNhTransform =>
        keywordNeighborhoodTransform.toJson
    }

    def read(value: JsValue): NeighborhoodTransform = value match {
      case JsString(typeid) => typeid match {
        case "ArcLabelNeighborhoodTransform" => ArclabelNhTransform
        case "DirectionNeighborhoodTransform" => DirectionNhTransform
        case "CardinalityNhTransform" => CardinalityNhTransform
        case x => deserializationError(s"Invalid identifier for NeighborhoodExtractor: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        propertyNhTransformFormat,
        suffixNeighborhoodTransformFormat, keywordNeighborhoodTransformFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** Maps the tokens of a neighborhood to a particular property in their token's property map.
  *
  * @param propertyName name of the desired property
  */
case class PropertyNhTransform(propertyName: Symbol) extends NeighborhoodTransform {
  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    Seq(
      FeatureName(event.tokens map { tok =>
        parse.tokens(tok).getDeterministicProperty(propertyName)
      })
    )
  }
}

/** Creates a feature for every suffix (from a dictionary of suffixes) that appears in
  * the input neighborhood.
  *
  * @param keysuffixes the set of suffixes to consider
  */
case class SuffixNhTransform(keysuffixes: Seq[String])
    extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    event.tokens flatMap { tok =>
      keysuffixes filter { suffix =>
        parse.tokens(tok).word.name.toLowerCase.endsWith(suffix.toLowerCase)
      } map { suffix =>
        FeatureName(Seq(Symbol(suffix)))
      }
    }
  }
}

/** Creates a feature for every keyword (from a dictionary of keywords) that appears in
  * the input neighborhood.
  *
  * Note that the keyword matching is case-insensitive.
  *
  * @param keywords the set of words to consider
  */
case class KeywordNhTransform(keywords: Seq[String])
    extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    event.tokens flatMap { tok =>
      keywords filter { keyword =>
        parse.tokens(tok).word.name.toLowerCase == keyword.toLowerCase
      } map { suffix =>
        FeatureName(Seq(Symbol(suffix.toLowerCase)))
      }
    }
  }
}

/** Creates a feature for the label on the arc connecting two tokens in a two-token neighborhood.
  *
  * Note that the apply operator will throw an exception if the argument neighborhood does
  * not have exactly two tokens. It will also throw an exception if the parse does not
  * contains an arc between the two neighborhood nodes.
  */
case object ArclabelNhTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    require(
      event.tokens.size == 2,
      s"cannot call ArclabelNhTransform on a neighborhood with ${event.tokens.size} tokens"
    )
    require(
      parse.arcLabelByEndNodes.contains(Set(event.tokens(0), event.tokens(1))),
      s"there is no arc between token ${event.tokens(0)} and ${event.tokens(1)}"
    )
    Seq(
      FeatureName(Seq(
        parse.arcLabelByEndNodes(Set(event.tokens(0), event.tokens(1)))
      ))
    )
  }
}

/** Creates a feature describing the order of two tokens in a two-token neighborhood.
  *
  * Specifically, it will be 'L if the first token appears to the left of the second token in
  * the sentence. Otherwise, it will be 'R.
  *
  * Note that the apply operator will throw an exception if the argument neighborhood does
  * not have exactly two tokens. It will also throw an exception if the two tokens are
  * the same (i.e. neither appears to the left of the other).
  */
case object DirectionNhTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    require(
      event.tokens.size == 2,
      s"cannot call ArclabelNhTransform on a neighborhood with ${event.tokens.size} tokens"
    )
    require(
      event.tokens(0) != event.tokens(1),
      s"cannot call ArclabelNhTransform on a neighborhood with a duplicate token"
    )
    val direction = (event.tokens(0) < event.tokens(1)) match {
      case true => 'L
      case false => 'R
    }
    Seq(
      FeatureName(Seq(direction))
    )
  }
}

/** Creates a feature describing the cardinality of a neighborhood, i.e. the number of tokens
  * in the neighborhood.
  */
case object CardinalityNhTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    Seq(FeatureName(Seq(Symbol(event.tokens.size.toString))))
  }
}
