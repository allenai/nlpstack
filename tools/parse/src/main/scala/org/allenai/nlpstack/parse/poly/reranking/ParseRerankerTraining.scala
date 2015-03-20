package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.core.WordClusters
import org.allenai.nlpstack.parse.poly.decisiontree.{ EntropyGainMetric, RandomForestTrainer }
import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm.{ RerankingFunction, Reranker, Sculpture }
import org.allenai.nlpstack.parse.poly.ml._
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

case class PRTCommandLine(
  nbestFilenames: String = "", otherNbestFilename: String = "", parserFilename: String = "",
  goldParseFilename: String = "", dataSource: String = "", clustersPath: String = "",
  vectorFilenames: String = "", rerankerFilename: String = "", otherGoldParseFilename: String = ""
)

/** A toy command-line that shows the way towards possibly better parse reranking.
  *
  * This trains a "weirdness" classifier that learns to classify parse tree nodes as "weird"
  * or not "weird," and then reranks parses based on how many of their nodes are classified as
  * "weird."
  */
object ParseRerankerTraining {

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
    }
    val clArgs: PRTCommandLine =
      optionParser.parse(args, PRTCommandLine()).get
    val clusters: Seq[BrownClusters] = {
      if (clArgs.clustersPath != "") {
        clArgs.clustersPath.split(",") map { path =>
          BrownClusters.fromLiangFormat(path)
        }
      } else {
        Seq[BrownClusters]()
      }
    }

    val otherGoldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.otherGoldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )

    println("Creating training vectors.")
    val nbestSource: ParsePoolSource = FileBasedParsePoolSource(clArgs.nbestFilenames)
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val feature = new ParseNodeFeatureUnion(Seq(
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
      )),
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

    val trainingData = createTrainingData(goldParseSource, nbestSource, feature)

    println("Creating test vectors.")
    val otherNbestSource: ParsePoolSource = FileBasedParsePoolSource(clArgs.otherNbestFilename)
    val testData = createTrainingData(otherGoldParseSource, otherNbestSource, feature)
    testData.labeledVectors foreach { x => println(x) }

    println("Training classifier.")
    val trainer = new WrapperClassifierTrainer(
      new RandomForestTrainer(0, 20, 200, EntropyGainMetric(0))
    )
    val classifier: WrapperClassifier = trainer(trainingData)

    val rerankingFunction = WeirdParseNodeRerankingFunction(classifier, feature, 0.3)

    val parser: TransitionParser = TransitionParser.load(clArgs.parserFilename)
    val candidateParses: Iterator[PolytreeParse] = {
      (otherGoldParseSource.sentenceIterator map { sent =>
        parser.parse(sent)
      }).flatten
    }
    val scoredCandidates = candidateParses map { candidateParse =>
      (candidateParse, rerankingFunction(candidateParse, 0.0))
      //(candidateParse, candidateParse.sentence.size)
    }
    val sortedCandidates = scoredCandidates.toSeq sortBy { case (_, score) => score } map { case (cand, _) => cand }
    val percentagesToPlot = Seq(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    val scoringFunction = PathAccuracyScore(otherGoldParseSource, true, false)
    val pathAccuracies = percentagesToPlot map { percentage =>
      val candidateSubset = sortedCandidates.take((percentage * sortedCandidates.size).toInt)
      val numerator = (candidateSubset map { candidate => scoringFunction.getRatio(candidate)._1 }).sum
      val denominator = (candidateSubset map { candidate => scoringFunction.getRatio(candidate)._2 }).sum
      numerator.toFloat / denominator
    }
    (percentagesToPlot zip pathAccuracies) foreach {
      case (percentage, accuracy) =>
        println(s"$percentage: $accuracy")
    }

    println("Evaluating classifier.")
    evaluate(trainingData, classifier)
    evaluate(testData, classifier)

    /*
    println("Creating reranker.")
    val rerankingFunction = WeirdParseNodeRerankingFunction(classifier, feature, 0.3)
    val reranker: Reranker = new Reranker(rerankingFunction)

    println("Reranking.")
    val candidateParses =
      for {
        parsePool <- otherNbestSource.poolIterator
      } yield {
        val candidate: Option[Sculpture] = reranker(parsePool.toNbestList)
        candidate match {
          case Some(parse: PolytreeParse) => Some(parse)
          case _ => None
        }
      }
    val stats: Seq[ParseStatistic] = Seq(UnlabeledBreadcrumbAccuracy, PathAccuracy(false, true),
      PathAccuracy(true, true), PathAccuracy(false, false), PathAccuracy(true, false))
    stats foreach { stat => stat.reset() }
    ParseEvaluator.evaluate(candidateParses, otherGoldParseSource.parseIterator, stats)

    //val parser: TransitionParser = TransitionParser.load(clArgs.parserFilename)
    parser match {
      case rankingParser: RerankingTransitionParser =>
        val revisedConfig = rankingParser.config.copy(
          parsingNbestSize = 10,
          rerankingFunction = rerankingFunction
        )
        val revisedParser = RerankingTransitionParser(revisedConfig)
        TransitionParser.save(revisedParser, clArgs.rerankerFilename)
        ParseFile.parseTestSet(revisedParser, otherGoldParseSource)
    }
    */
  }

  def createTrainingData(goldParseSource: PolytreeParseSource, parsePools: ParsePoolSource,
    feature: ParseNodeFeature): TrainingData = {

    val goldParseMap: Map[String, PolytreeParse] = (goldParseSource.parseIterator map { parse =>
      (parse.sentence.asWhitespaceSeparatedString, parse)
    }).toMap
    val positiveExamples: Iterable[(FeatureVector, Int)] =
      (parsePools.poolIterator flatMap { parsePool =>
        val parse = parsePool.parses.head
        val goldParse = goldParseMap(parse._1.sentence.asWhitespaceSeparatedString)
        Range(0, goldParse.sentence.tokens.size) map { tokenIndex =>
          (feature(goldParse, tokenIndex), 1)
        }
      }).toIterable
    val negativeExamples: Iterable[(FeatureVector, Int)] = {
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
            val badTokens: Set[Int] =
              (candidateParse.labeledFamilies.toSet -- goldParse.labeledFamilies.toSet) map {
                case (node, family) =>
                  node
              }
            badTokens map { badToken =>
              (feature(candidateParse, badToken), 0)
            }
        }
      }
    }
    println(s"Found ${positiveExamples.size} positive examples " +
      s"and ${negativeExamples.size} negative examples")
    TrainingData((positiveExamples ++ negativeExamples).toIterable)
  }

  def evaluate(trainingData: TrainingData, classifier: WrapperClassifier) {
    val numCorrect = trainingData.labeledVectors count {
      case (vec, outcome) =>
        classifier.classify(vec) == outcome
    }
    val total = trainingData.labeledVectors.size
    println(numCorrect)
    println(total)
    println(s"Accuracy: ${numCorrect.toFloat / total}")
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
        println(s"Number of nodes: ${parse.sentence.size}")
        println(s"Number of weird nodes: ${getWeirdNodes(parse).size}")
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

/** A parse statistic that outputs weirdness statistics for a candidate parse.
  *
  * @param rerankingFunction the weirdness reranking function
  */
case class WeirdnessAnalyzer(rerankingFunction: WeirdParseNodeRerankingFunction)
    extends ParseStatistic {

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    if (candidateParse != None) {
      val scoringFunction = PathAccuracyScore(
        InMemoryPolytreeParseSource(Seq(goldParse)),
        ignorePunctuation = true, ignorePathLabels = false
      )
      scoringFunction.getRatio(candidateParse.get) match {
        case (correctIncrement, totalIncrement) =>
          if (totalIncrement > 0 && correctIncrement.toFloat / totalIncrement < 0.5) {
            println(s"sentence: ${goldParse.sentence.asWhitespaceSeparatedString}")
            println(s"candidate: ${candidateParse.get}")
            println(s"gold: $goldParse")
            val weirdGoldNodes = rerankingFunction.getWeirdNodes(goldParse)
            weirdGoldNodes foreach { node => println(s"Weird gold node: $node") }
            val weirdCandidateNodes = rerankingFunction.getWeirdNodes(candidateParse.get)
            weirdCandidateNodes foreach { node => println(s"Weird candidate node: $node") }
            println("")
          }
      }
    }
  }

  override def report(): Unit = {}

  override def reset(): Unit = {}
}
