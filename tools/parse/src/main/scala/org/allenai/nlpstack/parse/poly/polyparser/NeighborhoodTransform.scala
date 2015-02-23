package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.ml.{ FeatureName, BrownClusters }
import org.allenai.common.json._
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A NeighborhoodTransform maps a Neighborhood into an "event" (a sequence of strings).
  *
  * An example might help. Suppose that we have a neighborhood consisting of
  * (node, child1, child2), i.e. three nodes of a parse tree. A transform might map these
  * to the sequence of their POS tags, e.g. ("VERB", "NOUN", "NOUN").
  *
  */
trait NeighborhoodTransform extends ((PolytreeParse, Neighborhood) => Seq[FeatureName]) {
  val name: String
}

object NeighborhoodTransform {

  /** Boilerplate code to serialize an EventTransform to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM EventTransform, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object NeighborhoodTransformJsonFormat extends RootJsonFormat[NeighborhoodTransform] {

    implicit val brownTransformFormat =
      jsonFormat3(BrownTransform.apply).pack("type" -> "BrownTransform")
    //implicit val tokenPropTransformFormat =
    //  jsonFormat1(TokenPropTransform.apply).pack("type" -> "TokenPropertyTransform")

    implicit val suffixNeighborhoodTransformFormat =
      jsonFormat2(SuffixNeighborhoodTransform.apply).pack("type" -> "SuffixNeighborhoodTransform")
    implicit val keywordNeighborhoodTransformFormat =
      jsonFormat2(KeywordNeighborhoodTransform.apply).pack("type" -> "KeywordNeighborhoodTransform")

    def write(feature: NeighborhoodTransform): JsValue = feature match {
      case CposNeighborhoodTransform => JsString("CposNeighborhoodTransform")
      case ArcLabelNeighborhoodTransform => JsString("ArcLabelNeighborhoodTransform")
      case brownTransform: BrownTransform =>
        brownTransform.toJson
      //case tokenPropertyTransform: TokenPropTransform =>
      // tokenPropertyTransform.toJson
      case suffixNeighborhoodTransform: SuffixNeighborhoodTransform =>
        suffixNeighborhoodTransform.toJson
      case keywordNeighborhoodTransform: KeywordNeighborhoodTransform =>
        keywordNeighborhoodTransform.toJson
    }

    def read(value: JsValue): NeighborhoodTransform = value match {
      case JsString(typeid) => typeid match {
        case "CposNeighborhoodTransform" => CposNeighborhoodTransform
        case "ArcLabelNeighborhoodTransform" => ArcLabelNeighborhoodTransform
        case x => deserializationError(s"Invalid identifier for NeighborhoodExtractor: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        brownTransformFormat,
        suffixNeighborhoodTransformFormat, keywordNeighborhoodTransformFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** Maps the tokens of a neighborhood to a particular property in their token's property map. */
/*
case class TokenPropTransform(label: Symbol) extends NeighborhoodTransform {
  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    event.tokens map { tok => parse.tokens(tok).getDeterministicProperty(label).name }
  }

  @transient override val name = label.name
}
*/

/** Maps the tokens of a neighborhood to their respective Brown clusters.
  *
  * @param clusters the Brown clusters
  * @param k the maximum granularity we want to consider for a Brown cluster (i.e. the depth in
  * the Brown cluster tree)
  * @param name a label for the transform
  */
case class BrownTransform(clusters: BrownClusters, k: Int,
    override val name: String) extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    clusters.getAllClusters(Symbol(parse.tokens(event.tokens(0)).word.name.toLowerCase)) map {
      cluster => FeatureName(Seq(cluster))
    }
  }
}

case class SuffixNeighborhoodTransform(keysuffixes: Seq[String], override val name: String) extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    keysuffixes filter { suffix =>
      parse.tokens(event.tokens(0)).word.name.toLowerCase.endsWith(suffix.toLowerCase)
    } map { suffix =>
      FeatureName(Seq('keysuffix, Symbol(suffix)))
    }
  }
}

case class KeywordNeighborhoodTransform(keywords: Seq[String], override val name: String) extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    keywords filter { keyword =>
      parse.tokens(event.tokens(0)).word.name.toLowerCase() == keyword.toLowerCase()
    } map { keyword =>
      FeatureName(Seq('keyword, Symbol(keyword)))
    }
  }
}

case object CposNeighborhoodTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    Seq(
      FeatureName(Seq('cpos, parse.tokens(event.tokens(0)).getDeterministicProperty('cpos)))
    )
  }

  override val name: String = "cposTransform"
}

case object ArcLabelNeighborhoodTransform extends NeighborhoodTransform {

  override def apply(parse: PolytreeParse, event: Neighborhood): Seq[FeatureName] = {
    require(event.tokens.size == 2)
    val direction = (event.tokens(0) < event.tokens(1)) match {
      case true => 'R
      case false => 'L
    }
    Seq(
      FeatureName(Seq(
        'arclabel, parse.arcLabelByEndNodes(Set(event.tokens(0), event.tokens(1))), direction
      ))
    )
  }

  override val name: String = "arcLabel"
}
