package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.fsm.RerankingFunction
import org.allenai.nlpstack.parse.poly.ml.LinearModel
import scopt.OptionParser
import spray.json._


private case class PRTPTCommandLine(inputRerankerFilename: String = "",
  outputParserConfigFilename: String = "", coefficientFilename: String = "",
  parserConfigFilename: String = "")



object ParseRerankerTrainingPhaseTwo {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[PRTPTCommandLine]("ParseFile") {
      opt[String]('m', "modelfile") required () valueName ("<file>") action
        { (x, c) => c.copy(coefficientFilename = x) } text ("the file containing the" +
        " model coefficients")
      opt[String]('i', "inputfile") valueName ("<file>") action
        { (x, c) => c.copy(inputRerankerFilename = x) } text ("the file containing the" +
        " incomplete reranker")
      opt[String]('o', "outputfile") required () valueName ("<file>") action
        { (x, c) => c.copy(outputParserConfigFilename = x) } text ("where to write the" +
        " new parser configuration")
      opt[String]('c', "configfile") required () valueName ("<file>") action
        { (x, c) => c.copy(parserConfigFilename = x) } text ("the file containing the" +
        " original parser configuration")

    }
    val clArgs: PRTPTCommandLine =
      optionParser.parse(args, PRTPTCommandLine()).get

    val jsValue = Util.getJsValueFromFile(clArgs.inputRerankerFilename)
    jsValue match {
      case JsObject(values) =>
      case _ => deserializationError("Unexpected JsValue type. Must be " +
        "JsObject.")
    }
    val incompleteRerankingFunction = jsValue.convertTo[RerankingFunction]
    val parserConfig: ParserConfiguration = ParserConfiguration.load(clArgs.parserConfigFilename)

    val linearModel = LinearModel.loadLinearModel(clArgs.coefficientFilename)
    incompleteRerankingFunction match {
      case linearRerankingFunction: LinearParseRerankingFunction =>
        val completeRerankingFunction: RerankingFunction =
          linearRerankingFunction.copy(linearModel = Some(linearModel))
        val newParserConfig = parserConfig.copy(rerankingFunction = completeRerankingFunction,
          parsingNbestSize = 5)
        ParserConfiguration.save(newParserConfig, clArgs.outputParserConfigFilename)
      case _ =>
        println("WARNING: nothing written")
    }
  }

}
