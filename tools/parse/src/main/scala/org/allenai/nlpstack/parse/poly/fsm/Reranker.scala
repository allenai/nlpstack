package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.common.Resource
import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.reranking.{
  WeirdParseNodeRerankingFunction,
  LinearParseRerankingFunction
}

import reming.CompactPrinter
import reming.DefaultJsonProtocol._

import java.io.{ BufferedWriter, FileWriter, PrintWriter }

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
  private implicit val linearParseRerankingFunctionFormat =
    jsonFormat2(LinearParseRerankingFunction.apply)
  private implicit val weirdParseNodeRerankingFunctionFormat =
    jsonFormat3(WeirdParseNodeRerankingFunction.apply)
  private implicit val baseCostRerankingFunctionFormat =
    jsonFormat0(() => BaseCostRerankingFunction)

  implicit val rerankingFunctionJsonFormat = parentFormat[RerankingFunction](
    childFormat[LinearParseRerankingFunction, RerankingFunction],
    childFormat[WeirdParseNodeRerankingFunction, RerankingFunction],
    childFormat[BaseCostRerankingFunction.type, RerankingFunction]
  )

  def load(filename: String): RerankingFunction = Util.readFromFile[RerankingFunction](filename)

  def save(rerankingFunction: RerankingFunction, filename: String): Unit = {
    Resource.using(new PrintWriter(new BufferedWriter(new FileWriter(filename)))) { writer =>
      CompactPrinter.printTo(writer, rerankingFunction)
    }
  }
}

case object BaseCostRerankingFunction extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = baseCost
}
