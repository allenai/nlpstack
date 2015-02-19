package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.ml.BrownClusters
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
trait NeighborhoodTransform extends (Neighborhood => Seq[String]) {
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
    implicit val tokenPropTransformFormat =
      jsonFormat1(TokenPropTransform.apply).pack("type" -> "TokenPropertyTransform")

    implicit val suffixNeighborhoodTransformFormat =
      jsonFormat2(SuffixNeighborhoodTransform.apply).pack("type" -> "SuffixNeighborhoodTransform")
    implicit val keywordNeighborhoodTransformFormat =
      jsonFormat2(KeywordNeighborhoodTransform.apply).pack("type" -> "KeywordNeighborhoodTransform")

    def write(feature: NeighborhoodTransform): JsValue = feature match {
      case brownTransform: BrownTransform =>
        brownTransform.toJson
      case tokenPropertyTransform: TokenPropTransform =>
        tokenPropertyTransform.toJson
      case suffixNeighborhoodTransform: SuffixNeighborhoodTransform =>
        suffixNeighborhoodTransform.toJson
      case keywordNeighborhoodTransform: KeywordNeighborhoodTransform =>
        keywordNeighborhoodTransform.toJson
    }

    def read(value: JsValue): NeighborhoodTransform = value match {
      case jsObj: JsObject => jsObj.unpackWith(brownTransformFormat, tokenPropTransformFormat,
        suffixNeighborhoodTransformFormat, keywordNeighborhoodTransformFormat)
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

/** Maps the tokens of a neighborhood to a particular property in their token's property map. */
case class TokenPropTransform(label: Symbol) extends NeighborhoodTransform {
  override def apply(event: Neighborhood): Seq[String] = {
    event.tokens map { tok => tok.getDeterministicProperty(label).name }
  }

  @transient override val name = label.name
}

/** Maps the tokens of a neighborhood to their respective Brown clusters.
  *
  * @param clusters the Brown clusters
  * @param k the maximum granularity we want to consider for a Brown cluster (i.e. the depth in
  * the Brown cluster tree)
  * @param name a label for the transform
  */
case class BrownTransform(clusters: BrownClusters, k: Int,
    override val name: String) extends NeighborhoodTransform {

  override def apply(event: Neighborhood): Seq[String] = {
    event.tokens map { tok =>
      clusters.getKthCluster(Symbol(tok.word.name.toLowerCase), k).name
    }
  }
}

case class SuffixNeighborhoodTransform(keysuffixes: Seq[String], override val name: String) extends NeighborhoodTransform {

  override def apply(event: Neighborhood): Seq[String] = {
    event.tokens flatMap { tok =>
      keysuffixes filter { suffix => tok.word.name.toLowerCase.endsWith(suffix.toLowerCase) }
    }
  }
}

case class KeywordNeighborhoodTransform(keywords: Seq[String], override val name: String) extends NeighborhoodTransform {

  override def apply(event: Neighborhood): Seq[String] = {
    event.tokens flatMap { tok =>
      keywords filter { keyword => tok.word.name.toLowerCase() == keyword.toLowerCase() }
    }
  }
}
