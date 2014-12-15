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

  /** Save a parser configuration to a file.
    *
    * This will automatically append ".poly.json" to the specified prefix.
    *
    * param parserConfig the configuration to save
    * @param modelFilePrefix the file to save the configuration to
    */
  /*
  def save(parserConfig: ParserConfiguration, modelFilePrefix: String): Unit = {
    val jsObj = parserConfig.toJson
    val writer = new PrintWriter(new File(modelFilePrefix + ".poly.json"))
    try {
      writer.println(jsObj.prettyPrint)
    } finally {
      writer.close()
    }
  }
  */

  /** Load a parser configuration from a file.
    *
    * param filename the file contains the serialized parser configuration
    * @return the initialized parser configuration
    */
  /*
  def load(filename: String): ParserConfiguration = {
    using(new File(filename).toURI.toURL.openStream()) { loadFromStream }
  }

  def loadFromStream(stream: InputStream): ParserConfiguration = {
    println("Loading parser.")
    System.gc()
    val initialMemory: Double = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory()
    val result = convertJsValueToConfig(Util.getJsValueFromStream(stream))
    System.gc()
    val memoryAfterLoading: Double = Runtime.getRuntime.totalMemory -
      Runtime.getRuntime.freeMemory()
    println("Parser memory footprint: %.2f GB".format((memoryAfterLoading - initialMemory)
      / Math.pow(2.0, 30.0)))
    result
  }

  private def convertJsValueToConfig(jsVal: JsValue): ParserConfiguration = {
    jsVal match {
      case JsObject(values) =>
      case _ => deserializationError("Unexpected JsValue type. Must be " +
        "JsObject.")
    }
    jsVal.convertTo[ParserConfiguration]
  }
  */
}
