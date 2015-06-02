package org.allenai.nlpstack.parse.poly.polytagger

import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.decisiontree.{ OmnibusTrainer, ProbabilisticClassifierTrainer }
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.parse.poly.ml.{ GoogleUnigramCpos, GoogleUnigramPos }
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

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
    val trainingSource: TaggedSentenceSource =
      ParseDerivedTaggedSentenceSource(
        MultiPolytreeParseSource(trainingConfig.trainingPath.split(",") map { path =>
          InMemoryPolytreeParseSource.getParseSource(
            path,
            ConllX(true), trainingConfig.dataSource
          )
        }), propertyName = 'cpos
      )

    val tagger = performStandardTraining(trainingSource, Some("factorie"))

    // save postagger
    SimplePostagger.save(tagger, trainingConfig.outputPath)

    // evaluate postagger
    SentenceTagger.fullTaggingEvaluation(tagger, trainingConfig.testPath, ConllX(true),
      trainingConfig.dataSource, ParseFile.defaultOracleNbest)
  }

  def performStandardTraining(
    trainingSource: TaggedSentenceSource,
    baseTagger: Option[String]
  ): SimplePostagger = {

    val keywords = (WordClusters.keyWords map { _.toString() }) ++
      WordClusters.harvestFrequentWordsFromSentenceSource(trainingSource, 3)

    val taggers: Seq[SentenceTaggerInitializer] =
      Seq(
        LexicalPropertiesTaggerInitializer,
        KeywordTaggerInitializer(keywords),
        GoogleUnigramTaggerInitializer(GoogleUnigramCpos),
        GoogleUnigramTaggerInitializer(GoogleUnigramPos),
        WikiSetTaggerInitializer
      )

    val transitionSystemFactory: TransitionSystemFactory =
      PostaggerTransitionSystemFactory(taggers)

    val baseCostFunctionFactory: Option[StateCostFunctionFactory] = None

    println("Training tagger.")
    val classifierTrainer: ProbabilisticClassifierTrainer =
      new OmnibusTrainer()
    val trainingVectorSource = new SculptureTrainingVectorSource(
      trainingSource,
      transitionSystemFactory, baseCostFunctionFactory
    )
    val parsingCostFunctionFactory: StateCostFunctionFactory = {
      val trainer =
        new DTCostFunctionTrainer(classifierTrainer, transitionSystemFactory,
          trainingVectorSource, baseCostFunctionFactory)
      trainer.costFunctionFactory
    }
    SimplePostagger(parsingCostFunctionFactory, nbestSize = 3)
  }
}

