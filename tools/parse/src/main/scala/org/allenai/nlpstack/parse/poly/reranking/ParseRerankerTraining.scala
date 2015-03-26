package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.common.Config.EnhancedConfig
import org.allenai.datastore._
import org.allenai.nlpstack.parse.poly.core.WordClusters
import org.allenai.nlpstack.parse.poly.decisiontree.{ EntropyGainMetric, RandomForestTrainer }
import org.allenai.nlpstack.parse.poly.fsm.{ RerankingFunction, Sculpture }
import org.allenai.nlpstack.parse.poly.ml._
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

import com.typesafe.config.{ Config, ConfigFactory }
import java.io.{ File, PrintWriter }

case class PRTCommandLine(
  nbestFilenames: String = "", otherNbestFilename: String = "", parserFilename: String = "",
  goldParseFilename: String = "", dataSource: String = "", clustersPath: String = "",
  vectorFilenames: String = "", rerankerFilename: String = "", otherGoldParseFilename: String = "",
  taggersConfigPathOption: Option[String] = None,
  classifierOutputLogPathOption: Option[String] = None
)

/** A command-line that shows the way towards possibly better parse reranking.
  *
  * This trains a "weirdness" classifier that learns to classify parse tree nodes as "weird"
  * or not "weird," which can be used to rerank parses based on how many of their nodes
  * are classified as "weird."
  */
object ParseRerankerTraining {

  /** Structure to represent labeled vectors together with corresponding classifier results.
    */
  case class LabeledFeatureVectorClassified(
    labeledVectorPerParseFamily: LabeledFeatureVectorPerParseFamily,
    classifierOutput: Int
  )

  def main(args: Array[String]) {
    val optionParser = new OptionParser[PRTCommandLine]("ParseRerankerTraining") {
      opt[String]('n', "nbestfiles") required () valueName ("<file>") action
        { (x, c) => c.copy(nbestFilenames = x) } text ("the file containing the nbest lists")
      opt[String]('m', "othernbestfile") required () valueName ("<file>") action
        { (x, c) => c.copy(otherNbestFilename = x) } text ("the file containing the test nbest lists")
      opt[String]('g', "goldfile") required () valueName ("<file>") action
        { (x, c) => c.copy(goldParseFilename = x) } text ("the file containing the gold parses")
      opt[String]('h', "othergoldfile") required () valueName ("<file>") action
        { (x, c) => c.copy(otherGoldParseFilename = x) } text (
          "the file containing the other gold parses"
        )
      opt[String]('p', "parser") required () valueName ("<file>") action
        { (x, c) => c.copy(parserFilename = x) } text ("the file containing the JSON " +
          " configuration for the parser")
      opt[String]('c', "clusters") valueName ("<file>") action
        { (x, c) => c.copy(clustersPath = x) } text ("the path to the Brown cluster files " +
          "(in Liang format, comma-separated filenames)")
      opt[String]('o', "outputfile") required () valueName ("<file>") action
        { (x, c) => c.copy(rerankerFilename = x) } text ("where to write the incomplete" +
          " reranking function")
      opt[String]('v', "vectorfiles") required () valueName ("<file>") action
        { (x, c) => c.copy(vectorFilenames = x) } text ("where to write the training" +
          " vectors")
      opt[String]('d', "datasource") required () valueName ("<file>") action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
          "('datastore','local')") validate { x =>
            if (Set("datastore", "local").contains(x)) {
              success
            } else {
              failure(s"unsupported data source: ${x}")
            }
          }
      opt[String]('t', "feature-taggers-config") valueName ("<file>") action
        { (x, c) => c.copy(taggersConfigPathOption = Some(x)) } text ("the path to a config file" +
          "containing config information required for the required taggers. Currently contains" +
          "datastore location info to access Verbnet resources for the Verbnet tagger.")
      opt[String]('l', "classifier-output-log") valueName ("<string>") action
        { (x, c) => c.copy(classifierOutputLogPathOption = Some(x)) } text ("path of required " +
          "output log file containing False Positives and False Negatives from the classifier on " +
          "the test set")
    }
    val clArgs: PRTCommandLine =
      optionParser.parse(args, PRTCommandLine()).get

    println("Creating reranker.")

    // Read in taggers config file if specified. This will contain config info necessary to
    // initialize the required feature taggers (currently contais only Verbnet config).
    val taggersConfigOption =
      clArgs.taggersConfigPathOption map (x => ConfigFactory.parseFile(new File(x)))

    val verbnetTransformOption: Option[(String, VerbnetTransform)] = for {
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
      ("verbnet", VerbnetTransform(new Verbnet(verbnetPath.toString)))
    }
    val feature = defaultParseNodeFeature(verbnetTransformOption)
    val rerankingFunctionTrainer = RerankingFunctionTrainer(feature)

    val nbestSource: ParsePoolSource =
      InMemoryParsePoolSource(FileBasedParsePoolSource(clArgs.nbestFilenames).poolIterator)

    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val (rerankingFunction: RerankingFunction, classifier) =
      rerankingFunctionTrainer.trainRerankingFunction(goldParseSource, nbestSource)

    println("Evaluating test vectors.")
    val otherGoldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.otherGoldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val otherNbestSource: ParsePoolSource = FileBasedParsePoolSource(clArgs.otherNbestFilename)
    val testData =
      createLabeledFeatureVectors(otherGoldParseSource, otherNbestSource, feature)
    testData.labeledVectors foreach { x => println(x) }
    evaluate(testData, classifier, clArgs.classifierOutputLogPathOption)

    println("Saving reranking function.")
    RerankingFunction.save(rerankingFunction, clArgs.rerankerFilename)
  }

  def defaultParseNodeFeature(verbnetTransformOption: Option[(String, VerbnetTransform)]) = new ParseNodeFeatureUnion(Seq(
    TransformedNeighborhoodFeature(Seq(
      ("children", AllChildrenExtractor)
    ), Seq(
      ("card", CardinalityNhTransform)
    )),
    TransformedNeighborhoodFeature(Seq(
      ("self", SelfExtractor),
      ("parent", EachParentExtractor),
      ("child", EachChildExtractor),
      ("parent1", SpecificParentExtractor(0)),
      ("parent2", SpecificParentExtractor(1)),
      ("parent3", SpecificParentExtractor(2)),
      ("parent4", SpecificParentExtractor(3)),
      ("child1", SpecificChildExtractor(0)),
      ("child2", SpecificChildExtractor(1)),
      ("child3", SpecificChildExtractor(2)),
      ("child4", SpecificChildExtractor(3)),
      ("child5", SpecificChildExtractor(4))
    ), Seq(
      ("cpos", PropertyNhTransform('cpos)),
      ("suffix", SuffixNhTransform(WordClusters.suffixes.toSeq map { _.name })),
      ("keyword", KeywordNhTransform(WordClusters.stopWords.toSeq map { _.name }))
    ) ++ verbnetTransformOption),
    TransformedNeighborhoodFeature(Seq(
      ("parent1", SelfAndSpecificParentExtractor(0)),
      ("parent2", SelfAndSpecificParentExtractor(1)),
      ("parent3", SelfAndSpecificParentExtractor(2)),
      ("parent4", SelfAndSpecificParentExtractor(3)),
      ("child1", SelfAndSpecificChildExtractor(0)),
      ("child2", SelfAndSpecificChildExtractor(1)),
      ("child3", SelfAndSpecificChildExtractor(2)),
      ("child4", SelfAndSpecificChildExtractor(3)),
      ("child5", SelfAndSpecificChildExtractor(4))
    ), Seq(
      ("alabel", ArclabelNhTransform),
      ("direction", DirectionNhTransform)
    ))
  ))

  def createLabeledFeatureVectors(goldParseSource: PolytreeParseSource, parsePools: ParsePoolSource,
    feature: ParseNodeFeature): LabeledFeatureVectors = {

    println("Creating gold parse map.")
    val goldParseMap: Map[String, PolytreeParse] = (goldParseSource.parseIterator map { parse =>
      (parse.sentence.asWhitespaceSeparatedString, parse)
    }).toMap
    println("Creating positive examples.")
    val positiveExamples: Iterable[LabeledFeatureVectorPerParseFamily] =
      (parsePools.poolIterator flatMap { parsePool =>
        val parse = parsePool.parses.head
        val sentence = parse._1.sentence.asWhitespaceSeparatedString
        val goldParse = goldParseMap(sentence)
        goldParse.labeledFamilies map {
          labeledFamily =>
            new LabeledFeatureVectorPerParseFamily(
              sentence, labeledFamily, feature(goldParse, labeledFamily.node.tokenIndex), 1
            )
        }
      }).toIterable
    println("Creating negative examples.")
    val negativeExamples: Iterable[LabeledFeatureVectorPerParseFamily] = {
      Range(0, 4) flatMap { _ =>
        val parsePairs: Iterator[(PolytreeParse, PolytreeParse)] =
          parsePools.poolIterator flatMap { parsePool =>
            Range(0, 1) map { i =>
              val randomParse = parsePool.chooseRandomParse
              (randomParse, goldParseMap(randomParse.sentence.asWhitespaceSeparatedString))
            }
          }
        parsePairs flatMap {
          case (candidateParse, goldParse) =>
            // Utility function to take a family and filter out token details. Returns a tuple of
            // just the node index of the root node and the arc labels and child node indices for
            // the outgoing arcs.
            def labeledFamilyNoTokenInfo(labeledFamily: LabeledFamily): (Int, Set[(Symbol, Int)]) = {
              (
                labeledFamily.node.tokenIndex,
                (labeledFamily.childArcsForNode map { arc => (arc._1, arc._2.tokenIndex) }).toSet
              )
            }
            // Keep only families in the candidate parse that are not in the gold parse because
            // we are extracting "bad" families. Token property information
            // may not tally since the gold parse will not contain all token properties.
            val goldParseLabeledFamiliesNoTokenInfo: Set[(Int, Set[(Symbol, Int)])] =
              goldParse.labeledFamilies.map {
                labeledFamily => labeledFamilyNoTokenInfo(labeledFamily)
              }.toSet

            val badNodeFamilies: Set[LabeledFamily] = (candidateParse.labeledFamilies.filter(
              candidateLabeledFamily =>
                !goldParseLabeledFamiliesNoTokenInfo.contains(
                  labeledFamilyNoTokenInfo(candidateLabeledFamily)
                )
            )).toSet

            badNodeFamilies map { badNodeLabeledFamily =>
              new LabeledFeatureVectorPerParseFamily(
                candidateParse.sentence.asWhitespaceSeparatedString,
                badNodeLabeledFamily,
                feature(candidateParse, badNodeLabeledFamily.node.tokenIndex), 0
              )
            }
        }
      }
    }
    println(s"Found ${positiveExamples.size} positive examples " +
      s"and ${negativeExamples.size} negative examples")
    LabeledFeatureVectors((positiveExamples ++ negativeExamples).toIterable)
  }

  def evaluate(
    trainingData: LabeledFeatureVectors,
    classifier: WrapperClassifier,
    logFilePathOption: Option[String]
  ): Unit = {

    // Get classifier results for all the labeled vectors.
    val classifierResults = for {
      labeledVector <- trainingData.labeledVectors
    } yield {
      new LabeledFeatureVectorClassified(
        labeledVector, classifier.classify(labeledVector.featureVector)
      )
    }

    val correct = classifierResults.filter(
      labeledVectorClassified =>
        labeledVectorClassified.labeledVectorPerParseFamily.expectedOutcome ==
          labeledVectorClassified.classifierOutput
    )

    val falsePositives = classifierResults.filter(
      labeledVectorClassified =>
        (labeledVectorClassified.labeledVectorPerParseFamily.expectedOutcome == 0) &&
          (labeledVectorClassified.classifierOutput == 1)
    )

    val falseNegatives = classifierResults.filter(
      labeledVectorClassified =>
        (labeledVectorClassified.labeledVectorPerParseFamily.expectedOutcome == 1) &&
          (labeledVectorClassified.classifierOutput == 0)
    )

    val numCorrect = correct.size

    val total = trainingData.labeledVectors.size
    println(numCorrect)
    println(total)
    println(s"Accuracy: ${numCorrect.toFloat / total}")

    // If a log file path is specified, misclassified families and associated feature vectors
    // have to be logged.
    logFilePathOption match {
      case Some(logFilePath) =>
        {
          val pw = new PrintWriter(new File(logFilePath))
          pw.write("False Positives:\n")
          printFamiliesAndVectors(falsePositives, pw)
          pw.write("\n\nFalse Negatives:\n")
          printFamiliesAndVectors(falseNegatives, pw)
          pw.close
        }
      case _ =>
    }
  }

  def printFamiliesAndVectors(
    classifierResults: Iterable[LabeledFeatureVectorClassified], pw: PrintWriter
  ): Unit = {
    val sentenceResultsMap = classifierResults.groupBy(_.labeledVectorPerParseFamily.sentence)
    for ((sentence, classifierResults) <- sentenceResultsMap) {
      pw.write(s"\ns${sentence}\n")
      for (result <- classifierResults) {
        // Write Label Family out in the following format:
        //(<NodeIx>, <NodeTokenString>) -> (<ArcLabel>, (<ChildNodeIx>, <ChildNodeTokenString>)),
        // (<ArcLabel> (<ChildNodeIx>, <ChildNodeTokenString>)), ...
        val labeledFamily = result.labeledVectorPerParseFamily.family
        pw.write("(" + labeledFamily.node.tokenIndex + ", " + labeledFamily.node.token.word.name +
          ") " + " -> ")
        val familyStrs = labeledFamily.childArcsForNode map (family =>
          "(" + family._1.name + ", " + "(" + family._2.tokenIndex + ", " +
            family._2.token.word.name + "))")
        pw.write(familyStrs.mkString(", "))
        // Write Feature Vector out, separating it from the family output by a tab.
        pw.write("\t" + result.labeledVectorPerParseFamily.featureVector.toString + "\n")
      }
    }
  }
}

case class RerankingFunctionTrainer(parseNodeFeature: ParseNodeFeature) {

  def trainRerankingFunction(
    goldParseSource: PolytreeParseSource,
    nbestSource: ParsePoolSource
  ): (RerankingFunction, WrapperClassifier) = {

    println("Creating training vectors.")
    val trainingData = ParseRerankerTraining.createLabeledFeatureVectors(goldParseSource, nbestSource, parseNodeFeature)
    //trainingData.labeledVectors foreach { case (vec, label) =>
    //  println(vec)
    //}
    println("Training classifier.")
    val trainer = new WrapperClassifierTrainer(
      new RandomForestTrainer(0, 10, 200, EntropyGainMetric(0))
    )
    val classifier: WrapperClassifier = trainer(trainingData)
    println("Evaluating classifier.")
    ParseRerankerTraining.evaluate(trainingData, classifier, None)
    (WeirdParseNodeRerankingFunction(classifier, parseNodeFeature, 0.3), classifier)
  }

}

/** This reranking function attempts to rerank parses based on how many "weird" nodes they have,
  * according to a "weirdness" classifier.
  *
  * @param classifier the weirdness classifier
  * @param feature computes a feature vector from a parse tree node
  * @param weirdnessThreshold the minimum probability of weirdness in order for a node to count as
  * "weird"
  */
case class WeirdParseNodeRerankingFunction(
    classifier: WrapperClassifier,
    feature: ParseNodeFeature,
    weirdnessThreshold: Double
) extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    val result = sculpture match {
      case parse: PolytreeParse => {
        getWeirdNodes(parse).size.toDouble
      }
      case _ => Double.MaxValue
    }
    result
  }

  /** Gets the set of "weird" tokens in a parse.
    *
    * @param parse the parse tree to analyze
    * @return the indices of all weird tokens
    */
  def getWeirdNodes(parse: PolytreeParse): Set[Int] = {
    val nodeWeirdness: Set[(Int, Double)] =
      Range(0, parse.tokens.size).toSet map { tokenIndex: Int =>
        (tokenIndex, classifier.getDistribution(feature(parse, tokenIndex)).getOrElse(0, 0.0))
      }
    nodeWeirdness filter {
      case (_, weirdness) =>
        weirdness >= weirdnessThreshold
    } map {
      case (node, _) =>
        node
    }
  }
}
