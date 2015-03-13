package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core.{ BrownClustersTagger, LexicalPropertiesTagger, FactorieSentenceTagger, SentenceTransform }
import org.allenai.nlpstack.parse.poly.decisiontree._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters
import scopt.OptionParser

private case class ParserTrainingConfig(
  clustersPath: String = "",
  trainingPath: String = "",
  outputPath: String = "", testPath: String = "", dataSource: String = ""
)

object Training {

  /** Command-line executable for training a parser from CoNLL-format training data.
    *
    * Usage: Training [options]
    *
    * -t <file> | --train <file>
    * the path to the training file (in ConllX format)
    * -o <file> | --output <file>
    * where to direct the output files
    * -x <file> | --test <file>
    * the path to the test file (in ConllX format)
    * -d <file> | --datasource <file>
    * the location of the data ('datastore','local')
    *
    * @param args command-line arguments (specified above)
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[ParserTrainingConfig]("Trainer") {
      opt[String]('t', "train") required () valueName ("<file>") action
        { (x, c) => c.copy(trainingPath = x) } text ("the path to the training files " +
          "(in ConllX format, comma-separated filenames)")
      opt[String]('c', "clusters") valueName ("<file>") action
        { (x, c) => c.copy(clustersPath = x) } text ("the path to the Brown cluster files " +
          "(in Liang format, comma-separated filenames)")
      opt[String]('o', "output") required () valueName ("<file>") action
        { (x, c) => c.copy(outputPath = x) } text ("where to direct the output files")
      opt[String]('x', "test") required () valueName ("<file>") action
        { (x, c) => c.copy(testPath = x) } text ("the path to the test file (in ConllX format)")
      opt[String]('d', "datasource") required () valueName ("<file>") action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
          "('datastore','local')") validate { x =>
            if (Set("datastore", "local").contains(x)) {
              success
            } else {
              failure("unsupported input format")
            }
          }
    }
    val config: ParserTrainingConfig = optionParser.parse(args, ParserTrainingConfig()).get
    val trainingSource: PolytreeParseSource =
      MultiPolytreeParseSource(config.trainingPath.split(",") map { path =>
        InMemoryPolytreeParseSource.getParseSource(
          path,
          ConllX(true), config.dataSource
        )
      })
    val clusters: Seq[BrownClusters] = {
      if (config.clustersPath != "") {
        config.clustersPath.split(",") map { path =>
          BrownClusters.fromLiangFormat(path)
        }
      } else {
        Seq[BrownClusters]()
      }
    }
    val taggers: Seq[SentenceTransform] =
      Seq(FactorieSentenceTagger, LexicalPropertiesTagger, BrownClustersTagger(clusters))
    val transitionSystemFactory: TransitionSystemFactory =
      ArcEagerTransitionSystemFactory(taggers)

    println("Training parser.")
    val classifierTrainer: ProbabilisticClassifierTrainer =
      new OmnibusTrainer()
    val trainingVectorSource = new GoldParseTrainingVectorSource(
      trainingSource,
      transitionSystemFactory, None
    )
    val parsingCostFunctionFactory: StateCostFunctionFactory = {
      val trainer =
        new DTCostFunctionTrainer(classifierTrainer, transitionSystemFactory,
          trainingVectorSource, None)
      trainer.costFunctionFactory
    }
    val parsingNbestSize = 5
    val parserConfig = ParserConfiguration(
      parsingCostFunctionFactory,
      BaseCostRerankingFunction, parsingNbestSize
    )
    val parser = RerankingTransitionParser(parserConfig)

    println("Saving models.")
    TransitionParser.save(parser, config.outputPath)

    ParseFile.fullParseEvaluation(parser, config.testPath, ConllX(true),
      config.dataSource, ParseFile.defaultOracleNbest)
  }
}
