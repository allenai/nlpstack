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
      new Reranker(OracleRerankingFunction(goldParseSource))
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
    val stats: Seq[ParseStatistic] = Seq(UnlabeledBreadcrumbAccuracy, PathAccuracy(false, false),
      PathAccuracy(false, true), PathAccuracy(true, false), PathAccuracy(true, true))
    stats foreach { stat => stat.reset() }
    ParseEvaluator.evaluate(candidateParses, goldParseSource.parseIterator, stats)
  }
}

case class OracleRerankingFunction(goldParses: PolytreeParseSource) extends RerankingFunction {

  val scoringFunction: ParseScore = PathAccuracyScore(
    goldParses,
    ignorePunctuation = true, ignorePathLabels = false
  )

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    sculpture match {
      case parse: PolytreeParse => 1.0 - scoringFunction(parse)
      case _ => 1.0
    }
  }
}
