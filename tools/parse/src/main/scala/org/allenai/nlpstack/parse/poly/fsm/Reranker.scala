package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.reranking.{
  WeirdParseNodeRerankingFunction,
  LinearParseRerankingFunction
}
import spray.json._
import spray.json.DefaultJsonProtocol._

/** Chooses the lowest cost parse from an n-best list (according to the reranking function).
  *
  * @param rerankingFunction the cost function that drives the reranker
  */
class Reranker(rerankingFunction: RerankingFunction) {

  def apply(nbestList: NbestList): Option[Sculpture] = {
    val rescoredCandidates: Seq[(Sculpture, Double)] = {
      (nbestList.scoredSculptures map {
        case (sculpture, baseCost) =>
          (sculpture, rerankingFunction(sculpture, baseCost))
      }).toSeq
    }
    val sortedCandidates = rescoredCandidates sortBy { _._2 } map { _._1 }
    sortedCandidates.headOption
  }

  def rerankWithScore(nbestList: NbestList): Option[(Sculpture, Double)] = {
    val rescoredCandidates: Seq[(Sculpture, Double)] = {
      (nbestList.scoredSculptures map {
        case (sculpture, baseCost) =>
          (sculpture, rerankingFunction(sculpture, baseCost))
      }).toSeq
    }
    val sortedCandidates = rescoredCandidates sortBy { _._2 }
    sortedCandidates.headOption
  }

  /** Applies the reranker to every nbest list in a corpus.
    *
    * @param nbestCorpus corpus of nbest lists
    * @return an iterator over the lowest cost candidate for every nbest list of the corpus
    */
  def rerankCorpus(nbestCorpus: NbestCorpus): Iterable[Option[Sculpture]] = {
    nbestCorpus.nbestLists map { nbestList => apply(nbestList) }
  }
}

/** A cost function for a pre-scored parse. */
abstract class RerankingFunction extends ((Sculpture, Double) => Double) {

  final def apply(sculpture: Sculpture): Double = this.apply(sculpture, 0.0)
}

object RerankingFunction {

  /** Boilerplate code to serialize a RerankingFunction to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM RerankingFunction, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object RerankingFunctionJsonFormat extends RootJsonFormat[RerankingFunction] {

    implicit val linearParseRerankingFunctionFormat =
      jsonFormat2(LinearParseRerankingFunction.apply).pack("type" -> "LinearParseRerankingFunction")
    implicit val weirdParseNodeRerankingFunctionFormat =
      jsonFormat3(WeirdParseNodeRerankingFunction.apply).pack(
        "type" -> "WeirdParseNodeRerankingFunction"
      )
    def write(rerankingFunction: RerankingFunction): JsValue = rerankingFunction match {
      case BaseCostRerankingFunction => JsString("BaseCostRerankingFunction")
      case linearParseRerankingFunction: LinearParseRerankingFunction =>
        linearParseRerankingFunction.toJson
      case weirdParseNodeRerankingFunction: WeirdParseNodeRerankingFunction =>
        weirdParseNodeRerankingFunction.toJson
    }
    def read(value: JsValue): RerankingFunction = value match {
      case JsString(typeid) => typeid match {
        case "BaseCostRerankingFunction" => BaseCostRerankingFunction
        case x => deserializationError(s"Invalid identifier for TaskIdentifier: $x")
      }
      case jsObj: JsObject => jsObj.unpackWith(
        linearParseRerankingFunctionFormat,
        weirdParseNodeRerankingFunctionFormat
      )
      case _ => deserializationError("Unexpected JsValue type. Must be JsString.")
    }
  }
}

case object BaseCostRerankingFunction extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = baseCost
}
