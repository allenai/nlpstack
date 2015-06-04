package org.allenai.nlpstack.parse.poly.polytagger

import org.allenai.nlpstack.parse.poly.core.{ StanfordPostaggerInitializer, FactoriePostaggerInitializer, SentenceTagger }
import org.allenai.nlpstack.parse.poly.eval.TaggingEvaluation
import org.allenai.nlpstack.parse.poly.polyparser.ConllX
import scopt.OptionParser

private case class RunPostaggerCommandLine(configFilename: String = "", testFilename: String = "",
  dataSource: String = "")

object RunPostagger {

  /** Simple command-line for contrastive evaluation of POS taggers.
    * format: OFF
    *
    * Usage: RunPostagger [options]
    *
    *   -c <file> | --config <file>
    *         the file containing the JSON configuration for the tagger
    *   -t <file> | --test <file>
    *         the file containing the gold parses to compare against (in ConllX format, comma-separated filenames)
    *   -d <file> | --datasource <file>
    *         the location of the data ('datastore','local')
    *
    * format: ON
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[RunPostaggerCommandLine]("RunPostagger") {
      opt[String]('c', "config") required () valueName "<file>" action { (x, c) =>
        c.copy(configFilename = x)
      } text "the file containing the JSON configuration for the tagger"
      opt[String]('t', "test") required () valueName "<file>" action { (x, c) =>
        c.copy(testFilename = x)
      } text ("the file containing the gold parses to compare against " +
        "(in ConllX format, comma-separated filenames)")
      opt[String]('d', "datasource") required () valueName "<file>" action { (x, c) =>
        c.copy(dataSource = x)
      } text ("the location of the data " +
        "('datastore','local')") validate { x =>
          if (Set("datastore", "local").contains(x)) {
            success
          } else {
            failure(s"unsupported data source: $x")
          }
        }
    }
    val config: RunPostaggerCommandLine = optionParser.parse(args, RunPostaggerCommandLine()).get

    println("Evaluating Stanford tagger:")
    val stanTagger =
      StanfordPostaggerInitializer(useCoarseTags = true).initialize()
    TaggingEvaluation.fullTaggingEvaluation(stanTagger, config.testFilename,
      ConllX(useGoldPOSTags = true), config.dataSource, 0)

    println("Evaluating Factorie tagger:")
    val factorieTagger =
      FactoriePostaggerInitializer(useCoarseTags = true).initialize()
    TaggingEvaluation.fullTaggingEvaluation(factorieTagger, config.testFilename,
      ConllX(useGoldPOSTags = true), config.dataSource, 0)

    println("Evaluating serialized tagger:")
    val tagger: SimplePostagger =
      SimplePostagger.load(config.configFilename, overrideNbestSize = Some(1))
    TaggingEvaluation.fullTaggingEvaluation(tagger, config.testFilename,
      ConllX(useGoldPOSTags = true), config.dataSource, 0)
  }
}

