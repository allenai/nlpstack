package org.allenai.nlpstack.parse.poly.postagging

import java.io.File

import org.allenai.common.Config.EnhancedConfig
import com.typesafe.config.{ Config, ConfigFactory }
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.decisiontree.{ OmnibusTrainer, ProbabilisticClassifierTrainer }
import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml._
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

    val maybeGoogleNgrams: Option[DatastoreGoogleNGram] = for {
      taggersConfig <- taggersConfigOption
      googleUnigramConfig <- taggersConfig.get[Config]("googleUnigram")
      groupName <- googleUnigramConfig.get[String]("group")
      artifactName <- googleUnigramConfig.get[String]("name")
      version <- googleUnigramConfig.get[Int]("version")
    } yield {
      new DatastoreGoogleNGram(groupName, artifactName, version, 1000)
    }
    val googleNgramTransforms: Seq[SentenceTransform] = maybeGoogleNgrams match {
      case Some(googleNgrams) =>
        Seq(GoogleUnigramCpos, GoogleUnigramPos) map { tagType =>
          GoogleUnigramTagger(googleNgrams, tagType)
        }
      case None =>
        Seq()
    }

    val taggers: Seq[SentenceTransform] =
      Seq(LexicalPropertiesTagger) ++ googleNgramTransforms ++
        Seq(WikiSetTagger(WikiSet("/Users/markhopkins/Projects/data/monolingual/enwiki-latest-all-titles-in-ns0")))

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
    val tagger = SimplePostagger(parsingCostFunctionFactory, nbestSize = 5)

    // save postagger
    SimplePostagger.save(tagger, trainingConfig.outputPath)

    // evaluate postagger
    PolyPostagger.fullTaggingEvaluation(tagger, trainingConfig.testPath, ConllX(true),
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
          (index, tok.getProperty('pos))
      }).toMap
      )
      vector <- generateVectors(taggedSentence)
    } yield {
      println(vector)
      vector
    }
  }
}
