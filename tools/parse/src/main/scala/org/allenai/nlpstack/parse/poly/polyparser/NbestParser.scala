package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ File, PrintWriter }

import org.allenai.nlpstack.parse.poly.core.{ Sentence, SentenceSource }
import org.allenai.nlpstack.parse.poly.fsm._
import spray.json._

import scopt.OptionParser

private case class NbestParserCommandLine(
  configFilename: String = "",
  inputFilename: String = "", inputFileFormat: String = "", outputFilename: String = "",
  nbestSize: Int = NbestParser.defaultNbestSize, dataSource: String = ""
)

object NbestParser {
  val defaultNbestSize: Int = 10

  /** Command-line for generating a ParseArchipelago from a parser and a corpus of sentences.
    *
    * Usage: NbestParser [options]
    * -c <file> | --config <file>
    * the file containing the JSON configuration for the parser
    * -i <file> | --input <file>
    * the file containing the sentences to parse
    * -f <file> | --inputformat <file>
    * the format of the input file
    * -o <file> | --output <file>
    * where to direct the parse pool corpus
    * -n <file> | --nbestsize <file>
    * maximum number of parses in each pool
    *
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[NbestParserCommandLine]("NbestParser") {
      opt[String]('c', "config") required () valueName ("<file>") action
        { (x, c) => c.copy(configFilename = x) } text ("the file containing the JSON " +
          "configuration for the parser")
      opt[String]('i', "input") required () valueName ("<file>") action
        { (x, c) => c.copy(inputFilename = x) } text ("the file containing the sentences " +
          "to parse")
      opt[String]('d', "datasource") required () valueName ("<file>") action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
          "('datastore','local')") validate { x =>
            if (Set("datastore", "local").contains(x)) {
              success
            } else {
              failure(s"unsupported data source: ${x}")
            }
          }
      opt[String]('o', "output") required () valueName ("<file>") action
        { (x, c) => c.copy(outputFilename = x) } text ("where to direct the nbest lists")
      opt[Int]('n', "nbestsize") required () valueName ("<int>") action
        { (x, c) => c.copy(nbestSize = x) } text ("maximum number of parses in each nbest list")
    }

    val clArgs: NbestParserCommandLine = optionParser.parse(args, NbestParserCommandLine()).get
    val sentenceSource: PolytreeParseSource =
      MultiPolytreeParseSource(clArgs.inputFilename.split(",") map { path =>
        InMemoryPolytreeParseSource.getParseSource(
          path,
          ConllX(true), clArgs.dataSource
        )
      })

    val parser: TransitionParser = TransitionParser.load(clArgs.configFilename)
    parser match {
      case rerankingParser: RerankingTransitionParser =>
        val parserConfig = rerankingParser.config.copy(parsingNbestSize = clArgs.nbestSize)
        val baseParser: NbestParser = new NbestParser(parserConfig)
        val candidateParses: Iterator[ParsePool] = {
          sentenceSource.sentenceIterator map {
            sentence =>
              ParsePool(baseParser.parse(sentence, Set()) map {
                case (parse, cost) =>
                  (parse, cost) //(PolytreeParse.arcInverterStanford(parse), cost)
              })
          }
        }
        FileBasedParsePoolSource.writePools(candidateParses, clArgs.outputFilename)
      case _ =>
        println("Cannot do n-best parsing without RerankingTransitionParser")
    }
  }
}

/** Gets the n-best greedy parses for a given sentence.
  *
  * @param config configuration for the parser
  */
class NbestParser(config: ParserConfiguration) {

  val baseParser: NbestSearch = new NbestSearch(config.parsingCostFunction)
  val reranker: Reranker = new Reranker(config.rerankingFunction)

  def parse(
    sentence: Sentence,
    constraints: Set[TransitionConstraint] = Set()
  ): Set[(PolytreeParse, Double)] = {

    val optNbestList: Option[NbestList] =
      config.parsingCostFunction.transitionSystem.initialState(
        sentence,
        constraints.toSeq
      ) map { initState =>
        baseParser.find(initState, config.parsingNbestSize, constraints)
      }
    val unlabeledParses: Iterable[(PolytreeParse, Double)] = optNbestList match {
      case None => Set()
      case Some(nbestList) => nbestList.scoredSculptures flatMap {
        case (sculpture, cost) =>
          sculpture match {
            case parse: PolytreeParse => Some((parse, cost))
            case _ => None
          }
      }
    }
    unlabeledParses.toSet
  }
}
