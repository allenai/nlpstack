package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ PrintWriter, File, InputStream }
import java.net.URL

import org.allenai.common.Resource._
import spray.json.DefaultJsonProtocol._
import org.allenai.nlpstack.parse.poly.fsm.{ StateCostFunctionFactory, RerankingFunction, StateCostFunction }
import org.allenai.nlpstack.parse.poly.core.Util
import spray.json._

/** Contains the key components of a parser (for serialization purposes).
  *
  * @param parsingCostFunctionFactory the cost function factory for the transition parser
  * @param rerankingFunction the cost function for parse reranking
  * @param parsingNbestSize the nbest size to generate for reranking
  */
case class ParserConfiguration(
  parsingCostFunctionFactory: StateCostFunctionFactory,
  rerankingFunction: RerankingFunction,
  parsingNbestSize: Int
)

object ParserConfiguration {
  implicit val jsFormat = jsonFormat3(ParserConfiguration.apply)
}
