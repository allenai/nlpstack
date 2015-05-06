package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.decisiontree.{
  OmnibusTrainer,
  ProbabilisticClassifierTrainer
}
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters
import org.allenai.nlpstack.parse.poly.postagging.FactoriePostaggerInitializer
import scopt.OptionParser

private case class AdaptiveTrainingConfig(baseModelPath: String = "", clustersPath: String = "",
  trainingPath: String = "", outputPath: String = "", testPath: String = "",
  dataSource: String = "")

object AdaptiveTraining {

  /** Command-line executable for adapting an existing parser to CoNLL-format training data.
    *
    * format: OFF
    * Usage: Trainer [options]
    *
    *   -b <file> | --base <file>
    *         the base parser to adapt
    *   -t <file> | --train <file>
    *         the path to the training files (in ConllX format, comma-separated filenames)
    *   -c <file> | --clusters <file>
    *         the path to the Brown cluster files (in Liang format, comma-separated filenames)
    *   -o <file> | --output <file>
    *         where to write the adapted parser
    *   -x <file> | --test <file>
    *         path to a test file (in ConllX format)
    *   -d <file> | --datasource <file>
    *         the location of the data ('datastore','local')
    * format: ON
    *
    * @param args command-line arguments (specified above)
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[AdaptiveTrainingConfig]("Trainer") {
      opt[String]('b', "base") required () valueName "<file>" action
        { (x, c) => c.copy(baseModelPath = x) } text "the base parser to adapt"
      opt[String]('t', "train") required () valueName "<file>" action
        { (x, c) => c.copy(trainingPath = x) } text ("the path to the training files " +
        "(in ConllX format, comma-separated filenames)")
      opt[String]('c', "clusters") valueName "<file>" action
        { (x, c) => c.copy(clustersPath = x) } text ("the path to the Brown cluster files " +
        "(in Liang format, comma-separated filenames)")
      opt[String]('o', "output") required () valueName "<file>" action
        { (x, c) => c.copy(outputPath = x) } text "where to write the adapted parser"
      opt[String]('x', "test") valueName "<file>" action
        { (x, c) => c.copy(testPath = x) } text "path to a test file (in ConllX format)"
      opt[String]('d', "datasource") required () valueName "<file>" action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
        "('datastore','local')") validate { x =>
        if (Set("datastore", "local").contains(x)) {
          success
        } else {
          failure("unsupported input format")
        }
      }
    }
    val config: AdaptiveTrainingConfig = optionParser.parse(args, AdaptiveTrainingConfig()).get
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
      Seq(PolyPostaggerSentenceTransform(FactoriePostaggerInitializer),
        LexicalPropertiesTagger, BrownClustersTagger(clusters))


    val transitionSystemFactory: TransitionSystemFactory =
      ArcHybridTransitionSystemFactory(taggers)

    println("Training parser.")
    val baseCostFunctionFactory: Option[StateCostFunctionFactory] =
      TransitionParser.load(config.baseModelPath) match {
        case rerankingParser: RerankingTransitionParser =>
          Some(rerankingParser.config.parsingCostFunctionFactory)
        case _ => None
      }

    val classifierTrainer: ProbabilisticClassifierTrainer =
      new OmnibusTrainer()
    val trainingVectorSource = new GoldParseTrainingVectorSource(
      trainingSource,
      transitionSystemFactory, baseCostFunctionFactory
    )
    val parsingCostFunctionFactory: StateCostFunctionFactory = {
      val trainer =
        new DTCostFunctionTrainer(classifierTrainer, transitionSystemFactory,
          trainingVectorSource, baseCostFunctionFactory)
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

    if (config.testPath != "") {
      ParseFile.fullParseEvaluation(parser, config.testPath, ConllX(true),
        config.dataSource, 1)
    }
  }
}
