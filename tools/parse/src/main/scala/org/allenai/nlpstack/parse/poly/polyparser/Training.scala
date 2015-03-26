package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.Config.EnhancedConfig
import org.allenai.datastore._
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.decisiontree._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ BrownClusters, Verbnet }
import scopt.OptionParser

import com.typesafe.config.{ Config, ConfigFactory }
import java.io.{ File, FileWriter, Writer }

private case class ParserTrainingConfig(
  clustersPath: Option[String] = None,
  taggersConfigPath: Option[String] = None,
  trainingPath: String = "",
  outputPath: String = "",
  testPath: String = "",
  dataSource: String = ""
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
        { (x, c) => c.copy(clustersPath = Some(x)) } text ("the path to the Brown cluster files " +
          "(in Liang format, comma-separated filenames)")
      opt[String]('n', "feature-taggers-config") valueName ("<file>") action
        { (x, c) => c.copy(taggersConfigPath = Some(x)) } text ("the path to a config file" +
          "containing config information required for the required taggers. Currently contains" +
          "datastore location info to access Verbnet resources for the Verbnet tagger.")
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
    val trainingConfig: ParserTrainingConfig = optionParser.parse(args, ParserTrainingConfig()).get
    val trainingSource: PolytreeParseSource =
      MultiPolytreeParseSource(trainingConfig.trainingPath.split(",") map { path =>
        InMemoryPolytreeParseSource.getParseSource(
          path,
          ConllX(true), trainingConfig.dataSource
        )
      })

    // Read in taggers config file if specified. This will contain config info necessary to
    // initialize the required feature taggers (currently contais only Verbnet config).
    val taggersConfigOption =
      trainingConfig.taggersConfigPath map (x => ConfigFactory.parseFile(new File(x)))

    val clusters: Seq[BrownClusters] = trainingConfig.clustersPath match {
      case Some(clustersPath) => clustersPath.split(",") map { path =>
        BrownClusters.fromLiangFormat(path)
      }
      case _ => Seq.empty[BrownClusters]
    }

    val verbnetTaggerOption: Option[VerbnetTagger] = for {
      taggersConfig <- taggersConfigOption
      verbnetConfig <- taggersConfig.get[Config]("verbnet")
      groupName <- verbnetConfig.get[String]("group")
      artifactName <- verbnetConfig.get[String]("name")
      version <- verbnetConfig.get[Int]("version")
    } yield {
      val verbnetPath: java.nio.file.Path = Datastore.directoryPath(
        groupName,
        artifactName,
        version
      )
      VerbnetTagger(new Verbnet(verbnetPath.toString))
    }

    val taggers: Seq[SentenceTransform] =
      Seq(FactorieSentenceTagger, LexicalPropertiesTagger,
        BrownClustersTagger(clusters)) ++ verbnetTaggerOption

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
    TransitionParser.save(parser, trainingConfig.outputPath)

    ParseFile.fullParseEvaluation(parser, trainingConfig.testPath, ConllX(true),
      trainingConfig.dataSource, ParseFile.defaultOracleNbest)
  }
}
