package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.decisiontree.{ OneVersusAllTrainer, ProbabilisticClassifierTrainer, RandomForestTrainer, DecisionTreeTrainer }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.BrownClusters
import scopt.OptionParser

private case class ParserTrainingConfig(baseModelPath: String = "", clustersPath: String = "",
  trainingPath: String = "",
  outputPath: String = "", testPath: String = "", dataSource: String = "")

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
      opt[String]('b', "base") valueName ("<file>") action
        { (x, c) => c.copy(baseModelPath = x) } text ("an optional base model file to adapt")
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

    val transitionSystem: TransitionSystem =
      ArcEagerTransitionSystem(ArcEagerTransitionSystem.defaultFeature, clusters)

    /*
    println("Training task tree.")
    val taskIdentifier: TaskIdentifier = {
      val taskActivationThreshold = 5000
      TaskConjunctionIdentifier.learn(
        List(
          ApplicabilitySignatureIdentifier,
          StateRefPropertyIdentifier(BufferRef(0), 'factorieCpos),
          StateRefPropertyIdentifier(StackRef(0), 'factorieCpos),
          StateRefPropertyIdentifier(BufferRef(0), 'factoriePos)),
        new GoldParseSource(trainingSource, transitionSystem),
        taskActivationThreshold)
    }
    */
    val taskIdentifier: TaskIdentifier = ApplicabilitySignatureIdentifier

    //val baseCostFunction: Option[ClassifierBasedCostFunction] =
    //  config.baseModelPath match {
    //    case "" => None
    //    case _ => Some(ClassifierBasedCostFunction.load(config.baseModelPath))
    //  }

    println("Training parser.")
    val baseCostFunction = None // TODO: fix this
    val trainingVectorSource = new GoldParseTrainingVectorSource(trainingSource, taskIdentifier,
      transitionSystem, baseCostFunction)
    println(s"Number of training vectors: ${trainingVectorSource.getVectorIterator.size}")
    var transitionHistogram = Map[StateTransition, Int]()
    trainingVectorSource.getVectorIterator foreach {
      case trainingVector =>
        transitionHistogram = transitionHistogram.updated(
          trainingVector.transition,
          1 + transitionHistogram.getOrElse(trainingVector.transition, 0)
        )
    }
    val transitionCounts = transitionHistogram.toSeq sortBy { case (transition, count) => count }
    transitionCounts foreach {
      case (transition, count) =>
        println(s"$transition: $count")
    }

    //val classifierTrainer: ProbabilisticClassifierTrainer = new RandomForestTrainer(0, 10, 100)
    //val classifierTrainer: ProbabilisticClassifierTrainer = new DecisionTreeTrainer(0.3)
    val classifierTrainer: ProbabilisticClassifierTrainer =
      new OneVersusAllTrainer(new RandomForestTrainer(0, 10, 100))
    val parsingCostFunction: StateCostFunction = {
      val trainer =
        new DTCostFunctionTrainer(
          classifierTrainer,
          taskIdentifier, transitionSystem, trainingVectorSource,
          baseCostFunction
        )
      trainer.costFunction
    }

    val parsingNbestSize = 15
    val parserConfig = ParserConfiguration(
      parsingCostFunction,
      BaseCostRerankingFunction, parsingNbestSize
    )
    val parser = RerankingTransitionParser(parserConfig)

    ParseFile.fullParseEvaluation(parser, config.testPath, ConllX(true),
      config.dataSource, 5)

    println("Saving models.")
    TransitionParser.save(parser, config.outputPath)

  }
}
