package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser.{ PolytreeParse, ConllX, PolytreeParseFileFormat }
import scopt.OptionParser

private case class EvaluateConfig(candidateFilename: String = "", goldFilename: String = "")

object Evaluate {

  /** Command-line for evaluating a set of parses against a gold set.
    *
    * Usage: Evaluate [options]
    *
    * -c <file> | --candidate <file>
    * the file containing the candidate parses (CoNLL-X format)
    * -g <file> | --gold <file>
    * the file containing the gold parses (CoNLL-X format)
    *
    * @param args see above
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[EvaluateConfig]("Evaluate") {
      opt[String]('c', "candidate") required () valueName ("<file>") action
        { (x, c) => c.copy(candidateFilename = x) } text ("the file containing the candidate " +
          "parses (CoNLL-X format)")
      opt[String]('g', "gold") required () valueName ("<file>") action
        { (x, c) => c.copy(goldFilename = x) } text ("the file containing the gold " +
          "parses (CoNLL-X format)")
    }
    val config: EvaluateConfig = optionParser.parse(args, EvaluateConfig()).get
    val fileFormat: PolytreeParseFileFormat = ConllX(true)
    val candidateParses: Iterator[Option[PolytreeParse]] = {
      PolytreeParse.fromFile(config.candidateFilename, fileFormat) map { Some(_) }
    }
    val goldParses: Iterator[PolytreeParse] = {
      PolytreeParse.fromFile(config.goldFilename, fileFormat)
    }
    val stats: Seq[ParseStatistic] = Seq(PathAccuracy(false, false, true), PathAccuracy(false, false),
      PathAccuracy(false, true), PathAccuracy(true, false), PathAccuracy(true, true),
      CposAccuracy(false))

    ParseEvaluator.evaluate(candidateParses, goldParses, stats)
  }
}
