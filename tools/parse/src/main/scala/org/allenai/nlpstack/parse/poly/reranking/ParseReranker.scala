package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm.{ Reranker, Sculpture, RerankingFunction }
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

case class ParseRerankerCommandLine(
  nbestFilenames: String = "",
  goldParseFilename: String = "",
  dataSource: String = "",
  rerankerFilename: String = ""
)

/** This command-line takes a serialized reranking function and uses it to rerank an n-best
  * list. The resulting parses are then evaluated against a gold set.
  */
object ParseReranker {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[ParseRerankerCommandLine]("ParseRerankerTraining") {
      opt[String]('n', "nbestfiles") required () valueName "<file>" action { (x, c) =>
        c.copy(nbestFilenames = x)
      } text "the file containing the nbest lists"
      opt[String]('g', "goldfile") required () valueName "<file>" action { (x, c) =>
        c.copy(goldParseFilename = x)
      } text "the file containing the gold parses"
      opt[String]('r', "reranker") required () valueName "<file>" action { (x, c) =>
        c.copy(rerankerFilename = x)
      } text "the file containing the serialized reranking function"
      opt[String]('d', "datasource") required () valueName "<file>" action { (x, c) =>
        c.copy(dataSource = x)
      } text "the location of the data ('datastore','local')" validate { x =>
        if (Set("datastore", "local").contains(x)) {
          success
        } else {
          failure(s"unsupported data source: $x")
        }
      }
    }
    val clArgs: ParseRerankerCommandLine =
      optionParser.parse(args, ParseRerankerCommandLine()).get
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val nbestSource: ParsePoolSource = FileBasedParsePoolSource(clArgs.nbestFilenames)

    println("Reranking.")
    val reranker: Reranker = new Reranker(RerankingFunction.load(clArgs.rerankerFilename))
    val candidateParses =
      for {
        parsePool <- nbestSource.poolIterator
      } yield {
        val candidate: Option[Sculpture] = reranker(parsePool.toNbestList)
        candidate match {
          case Some(parse: PolytreeParse) => Some(parse)
          case _ => None
        }
      }
    val stats: Seq[ParseStatistic] = Seq(UnlabeledBreadcrumbAccuracy, PathAccuracy(false, true),
      PathAccuracy(true, true), PathAccuracy(false, false), PathAccuracy(true, false))
    stats foreach { stat => stat.reset() }
    ParseEvaluator.evaluate(candidateParses, goldParseSource.parseIterator, stats)
  }
}

/** A parse statistic that outputs weirdness statistics for a candidate parse.
  *
  * @param rerankingFunction the weirdness reranking function
  */
case class WeirdnessAnalyzer(rerankingFunction: WeirdParseNodeRerankingFunction)
    extends ParseStatistic {

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    if (candidateParse != None) {
      val scoringFunction = PathAccuracyScore(
        InMemoryPolytreeParseSource(Seq(goldParse)),
        ignorePunctuation = true, ignorePathLabels = false
      )
      scoringFunction.getRatio(candidateParse.get) match {
        case (correctIncrement, totalIncrement) =>
          if (totalIncrement > 0 && correctIncrement.toFloat / totalIncrement < 0.5) {
            println(s"sentence: ${goldParse.sentence.asWhitespaceSeparatedString}")
            println(s"candidate: ${candidateParse.get}")
            println(s"gold: $goldParse")
            val weirdGoldNodes = rerankingFunction.getWeirdNodes(goldParse)
            weirdGoldNodes foreach { node => println(s"Weird gold node: $node") }
            val weirdCandidateNodes = rerankingFunction.getWeirdNodes(candidateParse.get)
            weirdCandidateNodes foreach { node => println(s"Weird candidate node: $node") }
            println("")
          }
      }
    }
  }

  override def report(): Unit = {}

  override def reset(): Unit = {}
}
