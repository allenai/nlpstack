package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm.{ BaseCostRerankingFunction, Sculpture, RerankingFunction }
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

case class QECommandLine(
  rerankerFilename: String = "",
  parserFilename: String = "",
  goldParseFilename: String = "",
  dataSource: String = ""
)

/** A command-line that plots an accuracy/yield curve, given a reranking function and a parser.
  */
object QualityEstimation {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[QECommandLine]("QualityEstimation") {
      opt[String]('r', "reranker") valueName "<file>" action { (x, c) =>
        c.copy(rerankerFilename = x)
      } text "a file containing a serialized reranking function (to compare to the baselines)"
      opt[String]('g', "goldfile") required () valueName "<file>" action { (x, c) =>
        c.copy(goldParseFilename = x)
      } text "the file containing the gold parses"
      opt[String]('p', "parser") required () valueName "<file>" action { (x, c) =>
        c.copy(parserFilename = x)
      } text "the file containing the serialized parser"
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
    val clArgs: QECommandLine =
      optionParser.parse(args, QECommandLine()).get
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )

    val scoringFunction = PathAccuracyScore(goldParseSource, true, false)

    val rerankingFunctions = {
      val baselines: Map[String, RerankingFunction] = Map(
        "oracle" ->
          ParseScoreRerankingFunction(scoringFunction),
        "sentence-length" ->
          SentenceLengthRerankingFunction,
        "model-score" ->
          BaseCostRerankingFunction
      )
      if (clArgs.rerankerFilename != "") {
        baselines.updated("serialized", RerankingFunction.load(clArgs.rerankerFilename))
      } else {
        baselines
      }
    }

    val parser: RerankingTransitionParser = TransitionParser.load(clArgs.parserFilename) match {
      case rerankParser: RerankingTransitionParser => rerankParser
    }

    val candidateParses: Iterable[(PolytreeParse, Double)] = {
      (goldParseSource.sentenceIterator map { sent =>
        parser.parseWithScore(sent)
      }).flatten
    }.toIterable

    for ((name, rerankingFunction) <- rerankingFunctions) {
      val scoredCandidates = candidateParses map {
        case (candidateParse, modelCost) =>
          (candidateParse, rerankingFunction(candidateParse, modelCost))
      }
      val sortedCandidates = scoredCandidates.toSeq sortBy { case (_, score) => score } map { case (cand, _) => cand }

      val percentagesToPlot = Seq(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
      val pathAccuracies = percentagesToPlot map { percentage =>
        val candidateSubset = sortedCandidates.take((percentage * sortedCandidates.size).toInt)
        val numerator = (candidateSubset map { candidate => scoringFunction.getRatio(candidate)._1 }).sum
        val denominator = (candidateSubset map { candidate => scoringFunction.getRatio(candidate)._2 }).sum
        numerator.toFloat / denominator
      }

      println(s"\n$name:")
      (percentagesToPlot zip pathAccuracies) foreach {
        case (percentage, accuracy) =>
          println(s"$percentage: $accuracy")
      }
    }
  }
}

private case object SentenceLengthRerankingFunction extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    sculpture match {
      case parse: PolytreeParse =>
        parse.sentence.size.toDouble
    }
  }
}

private case class ParseScoreRerankingFunction(scoringFunction: ParseScore) extends RerankingFunction {
  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    sculpture match {
      case parse: PolytreeParse =>
        -scoringFunction(parse)
    }
  }
}
