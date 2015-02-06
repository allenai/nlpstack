package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm._
import scopt.OptionParser

private case class OracleRerankerConfig(
  nbestCorpusFilename: String = "",
  goldParseFilename: String = "", dataSource: String = ""
)

object OracleReranker {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[OracleRerankerConfig]("ParseFile") {
      opt[String]('n', "nbestfile") required () valueName ("<file>") action
        { (x, c) => c.copy(nbestCorpusFilename = x) } text ("the file containing the nbest lists")
      opt[String]('g', "goldfile") required () valueName ("<file>") action
        { (x, c) => c.copy(goldParseFilename = x) } text ("the file containing the gold parses")
      opt[String]('d', "datasource") required () valueName ("<file>") action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
          "('datastore','local')") validate { x =>
            if (Set("datastore", "local").contains(x)) {
              success
            } else {
              failure(s"unsupported data source: ${x}")
            }
          }
    }
    val config: OracleRerankerConfig = optionParser.parse(args, OracleRerankerConfig()).get
    val parsePoolSource = FileBasedParsePoolSource(config.nbestCorpusFilename)
    val goldParseSource = FileBasedPolytreeParseSource.getParseSource(
      config.goldParseFilename,
      ConllX(true), config.dataSource
    )
    val reranker: Reranker =
      new Reranker(OracleRerankingFunction(goldParseSource.parseIterator))
    val candidateParses =
      for {
        parsePool <- parsePoolSource.poolIterator
      } yield {
        val candidate: Option[Sculpture] = reranker(parsePool.toNbestList)
        candidate match {
          case Some(parse: PolytreeParse) => Some(parse)
          case _ => None
        }
      }
    val stat = UnlabeledBreadcrumbAccuracy
    stat.reset()
    PathAccuracy.reset()
    ParseEvaluator.evaluate(candidateParses, goldParseSource.parseIterator,
      Set(stat, PathAccuracy))
  }
}

case class OracleRerankingFunction(goldParses: Iterator[PolytreeParse]) extends RerankingFunction {

  val scoringFunction: ParseScore = {
    val goldParseMap: Map[String, PolytreeParse] = (goldParses map { parse =>
      (parse.sentence.asWhitespaceSeparatedString, parse)
    }).toMap
    PathAccuracyScore(goldParseMap)
  }

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    val parse: Option[PolytreeParse] =
      sculpture match {
        case parse: PolytreeParse => Some(parse)
        case _ => None
      }
    1.0 - scoringFunction(parse)
  }
}
