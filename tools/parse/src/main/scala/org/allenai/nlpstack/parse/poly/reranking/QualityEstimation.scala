package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

case class QECommandLine(
  nbestFilenames: String = "",
  parserFilename: String = "",
  goldParseFilename: String = "",
  dataSource: String = "",
  clustersPath: String = "",
  otherGoldParseFilename: String = ""
)

/** A toy command-line that shows the way towards possibly better parse reranking.
  *
  * This trains a "weirdness" classifier that learns to classify parse tree nodes as "weird"
  * or not "weird," and then reranks parses based on how many of their nodes are classified as
  * "weird."
  */
object QualityEstimation {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[QECommandLine]("QualityEstimation") {
      opt[String]('n', "nbestfiles") required () valueName ("<file>") action { (x, c) => c.copy(nbestFilenames = x) } text ("the file containing the nbest lists")
      opt[String]('g', "goldfile") required () valueName ("<file>") action { (x, c) => c.copy(goldParseFilename = x) } text ("the file containing the gold parses")
      opt[String]('h', "othergoldfile") required () valueName ("<file>") action { (x, c) => c.copy(otherGoldParseFilename = x) } text (
        "the file containing the other gold parses"
      )
      opt[String]('p', "parser") required () valueName ("<file>") action { (x, c) => c.copy(parserFilename = x) } text ("the file containing the JSON " +
        " configuration for the parser")
      opt[String]('c', "clusters") valueName ("<file>") action { (x, c) => c.copy(clustersPath = x) } text ("the path to the Brown cluster files " +
        "(in Liang format, comma-separated filenames)")
      opt[String]('d', "datasource") required () valueName ("<file>") action { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
        "('datastore','local')") validate { x =>
          if (Set("datastore", "local").contains(x)) {
            success
          } else {
            failure(s"unsupported data source: ${x}")
          }
        }
    }
    val clArgs: QECommandLine =
      optionParser.parse(args, QECommandLine()).get
    val nbestSource: ParsePoolSource = FileBasedParsePoolSource(clArgs.nbestFilenames)
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val (rerankingFunction, classifier) =
      ParseRerankerTraining.trainRerankingFunction(goldParseSource, nbestSource)

    val parser: RerankingTransitionParser = TransitionParser.load(clArgs.parserFilename) match {
      case rerankParser: RerankingTransitionParser => rerankParser
    }

    val otherGoldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.otherGoldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val candidateParses: Iterator[(PolytreeParse, Double)] = {
      (otherGoldParseSource.sentenceIterator map { sent =>
        parser.parseWithScore(sent)
      }).flatten
    }
    val scoringFunction = PathAccuracyScore(otherGoldParseSource, true, false)
    //val scoredCandidates = candidateParses // model score
    val scoredCandidates = candidateParses map {
      case (candidateParse, modelCost) =>
        (candidateParse, rerankingFunction(candidateParse, 0.0)) // weird nodes
      //(candidateParse, candidateParse.sentence.size)  // sentence length
      //(candidateParse, -scoringFunction(candidateParse))  // oracle
    }
    val sortedCandidates = scoredCandidates.toSeq sortBy { case (_, score) => score } map { case (cand, _) => cand }

    val percentagesToPlot = Seq(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    val pathAccuracies = percentagesToPlot map { percentage =>
      val candidateSubset = sortedCandidates.take((percentage * sortedCandidates.size).toInt)
      val numerator = (candidateSubset map { candidate => scoringFunction.getRatio(candidate)._1 }).sum
      val denominator = (candidateSubset map { candidate => scoringFunction.getRatio(candidate)._2 }).sum
      numerator.toFloat / denominator
    }

    (percentagesToPlot zip pathAccuracies) foreach {
      case (percentage, accuracy) =>
        println(s"$percentage: $accuracy")
    }
  }
}
