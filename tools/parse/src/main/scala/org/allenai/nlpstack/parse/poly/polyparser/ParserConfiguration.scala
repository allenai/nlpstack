package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ PrintWriter, File, InputStream }
import java.net.URL

import org.allenai.common.Resource._
import spray.json.DefaultJsonProtocol._
import org.allenai.nlpstack.parse.poly.fsm.{ RerankingFunction, StateCostFunction }
import org.allenai.nlpstack.parse.poly.core.Util
import spray.json._

/** Contains the key components of a parser (for serialization purposes).
  *
  * @param parsingCostFunction the cost function for the transition parser
  * param labelingCostFunction the cost function for the arc labeler
  * @param rerankingFunction the cost function for parse reranking
  * @param parsingNbestSize the nbest size to generate for reranking
  */
case class ParserConfiguration(parsingCostFunction: StateCostFunction,
  //labelingCostFunction: StateCostFunction,
  rerankingFunction: RerankingFunction,
  parsingNbestSize: Int)

object ParserConfiguration {
  implicit val jsFormat = jsonFormat3(ParserConfiguration.apply)
}
