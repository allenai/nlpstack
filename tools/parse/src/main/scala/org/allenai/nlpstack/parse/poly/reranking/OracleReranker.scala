package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

private case class OracleRerankerConfig(
  nbestCorpusFilename: String = "",
  goldParseFilename: String = "", dataSource: String = ""
)

object OracleReranker {

  /** Command-line for reranking a set of n-best lists according to an oracle score.
    *
    * The oracle score is the labeled path accuracy (ignoring punctuation) with respect to a
    * gold parse.
    *
    * format: OFF
    * Usage: ParseFile [options]
    *
    *    -n <file> | --nbestfile <file>
    *          the file containing the nbest lists
    *    -g <file> | --goldfile <file>
    *          the file containing the gold parses
    *    -d <file> | --datasource <file>
    *          the location of the data ('datastore','local')
    * format: ON
    *
    * @param args see above
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[OracleRerankerConfig]("ParseFile") {
      opt[String]('n', "nbestfile") required () valueName "<file>" action
        { (x, c) => c.copy(nbestCorpusFilename = x) } text "the file containing the nbest lists"
      opt[String]('g', "goldfile") required () valueName "<file>" action
        { (x, c) => c.copy(goldParseFilename = x) } text "the file containing the gold parses"
      opt[String]('d', "datasource") required () valueName "<file>" action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
          "('datastore','local')") validate { x =>
            if (Set("datastore", "local").contains(x)) {
              success
            } else {
              failure(s"unsupported data source: $x")
            }
          }
    }
    val config: OracleRerankerConfig = optionParser.parse(args, OracleRerankerConfig()).get
    val parsePoolSource = FileBasedParsePoolSource(config.nbestCorpusFilename)
    val goldParseSource = FileBasedPolytreeParseSource.getParseSource(
      config.goldParseFilename,
      ConllX(true), config.dataSource
    )

    val oracleScore: ParseScore = PathAccuracyScore(
      goldParseSource,
      ignorePunctuation = true, ignorePathLabels = false, useCrumbOnly = false
    )
    val reranker: Reranker =
      new Reranker(ParseRerankingFunction(oracleScore))
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
    val stats: Seq[ParseStatistic] = Seq(PathAccuracy(false, false, true), PathAccuracy(false, false),
      PathAccuracy(false, true), PathAccuracy(true, false), PathAccuracy(true, true))
    stats foreach { stat => stat.reset() }
    ParseEvaluator.evaluate(candidateParses, goldParseSource.parseIterator, stats)
  }
}
