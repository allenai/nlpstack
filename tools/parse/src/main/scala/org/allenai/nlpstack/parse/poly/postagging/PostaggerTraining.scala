package org.allenai.nlpstack.parse.poly.postagging

import java.io.File

import org.allenai.common.Config.EnhancedConfig
import com.typesafe.config.{ Config, ConfigFactory }
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.decisiontree.{ OmnibusTrainer, ProbabilisticClassifierTrainer }
import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ GoogleNGram, Verbnet, BrownClusters }
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

import scala.compat.Platform
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

private case class PostaggerTrainingCommandLine(
  taggersConfigPath: Option[String] = None,
  trainingPath: String = "",
  outputPath: String = "",
  testPath: String = "",
  dataSource: String = ""
)

object PostaggerTraining {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[PostaggerTrainingCommandLine]("PostaggerTrainer") {
      opt[String]('t', "train") required () valueName "<file>" action
        { (x, c) => c.copy(trainingPath = x) } text ("the path to the training files " +
          "(in ConllX format, comma-separated filenames)")
      opt[String]('n', "feature-taggers-config") valueName "<file>" action
        { (x, c) => c.copy(taggersConfigPath = Some(x)) } text ("the path to a config file" +
          "containing config information required for the required taggers. Currently contains" +
          "datastore location info to access Verbnet resources for the Verbnet tagger.")
      opt[String]('o', "output") required () valueName "<file>" action
        { (x, c) => c.copy(outputPath = x) } text "where to direct the output files"
      opt[String]('x', "test") required () valueName "<file>" action
        { (x, c) => c.copy(testPath = x) } text "the path to the test file (in ConllX format)"
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
    val trainingConfig: PostaggerTrainingCommandLine =
      optionParser.parse(args, PostaggerTrainingCommandLine()).get
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

    val googleUnigramTransformOption: Option[GoogleUnigramTagger] = for {
      taggersConfig <- taggersConfigOption
      googleUnigramConfig <- taggersConfig.get[Config]("googleUnigram")
      groupName <- googleUnigramConfig.get[String]("group")
      artifactName <- googleUnigramConfig.get[String]("name")
      version <- googleUnigramConfig.get[Int]("version")
    } yield {
      GoogleUnigramTagger(new GoogleNGram(groupName, artifactName, version, 1000))
    }

    val taggers: Seq[SentenceTransform] =
      Seq(FactorieSentenceTagger, LexicalPropertiesTagger) ++ googleUnigramTransformOption

    val transitionSystemFactory: TransitionSystemFactory =
      PostaggerTransitionSystemFactory(taggers)

    println("Training tagger.")
    val classifierTrainer: ProbabilisticClassifierTrainer =
      new OmnibusTrainer()
    val trainingVectorSource = new GoldTagsTrainingVectorSource(
      trainingSource,
      transitionSystemFactory, None
    )
    val parsingCostFunctionFactory: StateCostFunctionFactory = {
      val trainer =
        new DTCostFunctionTrainer(classifierTrainer, transitionSystemFactory,
          trainingVectorSource, None)
      trainer.costFunctionFactory
    }
    val tagger = Postagger(parsingCostFunctionFactory, BaseCostRerankingFunction, 5)

    // save postagger

    // evaluate postagger
    def fullTaggingEvaluation(parser: Postagger, testFiles: String,
      testFileFormat: PolytreeParseFileFormat, dataSource: String,
      oracleNbestSize: Int): Unit = {

      val testSources: Map[String, PolytreeParseSource] =
        (testFiles.split(",") map { path =>
          (path, FileBasedPolytreeParseSource.getParseSource(
            path,
            testFileFormat, dataSource
          ))
        }).toMap
      for ((sourcePath, testSource) <- testSources) {
        println(s"Checking tagging accuracy on test set ${sourcePath}.")
        evaluateTaggerOnTestSet(tagger, testSource)
      }
    }

    def tagTestSet(
      tagger: Postagger,
      sentenceSource: SentenceSource
    ): Iterator[Option[TaggedSentence]] = {

      import scala.concurrent.ExecutionContext.Implicits.global
      val taggingTasks: Iterator[Future[Option[TaggedSentence]]] =
        for {
          sentence <- sentenceSource.sentenceIterator
        } yield Future {
          tagger.tag(sentence)
        }
      val futureTagged: Future[Iterator[Option[TaggedSentence]]] = Future.sequence(taggingTasks)
      Await.result(futureTagged, 2 days)
    }

    def evaluateTaggerOnTestSet(parser: Postagger, parseSource: PolytreeParseSource): Unit = {
      println("Tagging test set.")
      val startTime: Long = Platform.currentTime
      val candidateTaggedSentences = tagTestSet(parser, parseSource) map { maybeTaggedSentence =>
        maybeTaggedSentence map { taggedSentence =>
          taggedSentence.addTagsToSentenceProperties('cpos)
        }
      }
      val stats: Seq[EvaluationStatistic] = Seq(
        CposSentAccuracy(false)
      )
      stats foreach { stat => stat.reset() }
      TaggingEvaluator.evaluate(candidateTaggedSentences, parseSource.sentenceIterator, stats)
      val parsingDurationInSeconds: Double = (Platform.currentTime - startTime) / 1000.0
      println("Parsed %d sentences in %.1f seconds, an average of %.1f sentences per second.".format(
        UnlabeledBreadcrumbAccuracy.numParses, parsingDurationInSeconds,
        (1.0 * UnlabeledBreadcrumbAccuracy.numParses) / parsingDurationInSeconds
      ))
    }

    fullTaggingEvaluation(tagger, trainingConfig.testPath, ConllX(true),
      trainingConfig.dataSource, ParseFile.defaultOracleNbest)
  }
}

case class GoldTagsTrainingVectorSource(
  goldParses: PolytreeParseSource,
  transitionSystemFactory: TransitionSystemFactory,
  baseCostFunctionFactory: Option[StateCostFunctionFactory] = None
)
    extends FSMTrainingVectorSource(transitionSystemFactory, baseCostFunctionFactory) {

  def getVectorIterator: Iterator[FSMTrainingVector] = {
    for {
      goldSentence <- goldParses.sentenceIterator
      taggedSentence = TaggedSentence(
        goldSentence,
        (goldSentence.tokens.zipWithIndex map {
        case (tok, index) =>
          (index, tok.getDeterministicProperty('cpos))
      }).toMap
      )
      vector <- generateVectors(taggedSentence)
    } yield {
      //println(vector)
      vector
    }
  }
}
