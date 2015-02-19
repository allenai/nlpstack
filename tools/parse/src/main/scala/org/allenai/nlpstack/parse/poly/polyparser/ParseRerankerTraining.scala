package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ File, PrintWriter }

import org.allenai.nlpstack.parse.poly.core.WordClusters
import org.allenai.nlpstack.parse.poly.decisiontree.RandomForestTrainer
import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm.{ Reranker, Sculpture, RerankingFunction }
import org.allenai.nlpstack.parse.poly.ml._
import scopt.OptionParser

private case class PRTCommandLine(
  nbestFilenames: String = "", otherNbestFilename: String = "", parserFilename: String = "",
  goldParseFilename: String = "", dataSource: String = "", clustersPath: String = "",
  vectorFilenames: String = "", rerankerFilename: String = "", otherGoldParseFilename: String = ""
)
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

    println("Initializing features.")
    //val transforms = Seq(
    //("brown", BrownTransform(clusters.head, 100, "brown")),
    //("arclabel", TokenPropTransform('arclabel)),
    //  ("cpos", TokenPropTransform('cpos))
    //)

    println("Creating training vectors.")
    val nbestSource: ParsePoolSource = FileBasedParsePoolSource(clArgs.nbestFilenames)
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val feature = new ParseNodeFeatureUnion(Seq(
      TransformedNeighborhoodFeature(Seq(
        ("self", SelfExtractor),
        ("children", ChildrenExtractor),
        ("parent1", SingleParentExtractor(0)),
        ("parent2", SingleParentExtractor(1)),
        ("child1", SingleChildExtractor(1)),
        ("child2", SingleChildExtractor(2)),
        ("child3", SingleChildExtractor(3)),
        ("child4", SingleChildExtractor(4)),
        ("child5", SingleChildExtractor(5))
      ), Seq(
        ("cpos", TokenPropTransform('cpos))
      //("brown", BrownTransform(clusters.head, 100, "brown"))
      )),
      TransformedNeighborhoodFeature(Seq(
        ("self", SelfExtractor),
        ("parent1", SingleParentExtractor(0)),
        ("parent2", SingleParentExtractor(1)),
        ("child1", SingleChildExtractor(1)),
        ("child2", SingleChildExtractor(2)),
        ("child3", SingleChildExtractor(3)),
        ("child4", SingleChildExtractor(4)),
        ("child5", SingleChildExtractor(5))
      ), Seq(
        ("suffix", SuffixNeighborhoodTransform(WordClusters.suffixes.toSeq map { _.name }, "suffix")),
        ("keyword", KeywordNeighborhoodTransform(WordClusters.stopWords.toSeq map { _.name }, "keyword"))
      )),
      ChildrenArcLabelsFeature
    //TransformedNeighborhoodHistogramFeature(Seq(
    //  ("children", ChildrenExtractor)
    //), transforms, goldParseSource)
    ))

    val trainingData = createTrainingData(goldParseSource, nbestSource, feature)

    println("Creating test vectors.")
    val otherNbestSource: ParsePoolSource = FileBasedParsePoolSource(clArgs.otherNbestFilename)
    val otherGoldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.otherGoldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val testData = createTrainingData(otherGoldParseSource, otherNbestSource, feature)
    testData.labeledVectors foreach { x => println(x) }

    println("Training classifier.")
    val trainer = new WrapperClassifierTrainer(new RandomForestTrainer(0, 10, 100))
    val classifier: WrapperClassifier = trainer(trainingData)
    evaluate(trainingData, classifier)
    evaluate(testData, classifier)

    println("Creating reranker.")
    val rerankingFunction = WrapperClassifierRerankingFunction(classifier, feature)
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
    val stat = UnlabeledBreadcrumbAccuracy
    stat.reset()
    PathAccuracy.reset()
    ParseEvaluator.evaluate(candidateParses, otherGoldParseSource.parseIterator,
      Set(stat, PathAccuracy, MistakeAnalyzer(classifier, feature)))

    val parser: TransitionParser = TransitionParser.load(clArgs.parserFilename)
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
  }

  def createTrainingData(goldParseSource: PolytreeParseSource, parsePools: ParsePoolSource,
    feature: ParseNodeFeature): TrainingData = {

    val goldParseMap: Map[String, PolytreeParse] = (goldParseSource.parseIterator map { parse =>
      (parse.sentence.asWhitespaceSeparatedString, parse)
    }).toMap
    val parsePairs: Iterator[(PolytreeParse, PolytreeParse)] =
      parsePools.poolIterator flatMap { parsePool =>
        Range(0, 1) map { i =>
          val randomParse = parsePool.chooseRandomParse
          (randomParse, goldParseMap(randomParse.sentence.asWhitespaceSeparatedString))
        }
      }
    val trainingData = TrainingData((parsePairs flatMap {
      case (candidateParse, goldParse) =>
        val badTokens: Set[Int] =
          (candidateParse.families.toSet -- goldParse.families.toSet) map { family =>
            family.head
          }
        badTokens flatMap { badToken =>
          Seq((feature(candidateParse, badToken), 0), (feature(goldParse, badToken), 1))
        }
    }).toIterable)
    trainingData
  }

  def evaluate(trainingData: TrainingData, classifier: WrapperClassifier) {
    var correctCounter = 0
    val total = trainingData.labeledVectors.size
    trainingData.labeledVectors foreach {
      case (vec, outcome) =>
        if (classifier.classify(vec) == outcome) {
          correctCounter += 1
        }
    }
    println(correctCounter)
    println(total)
    println(s"Accuracy: ${correctCounter.toFloat / total}")
  }
}

case class WrapperClassifierRerankingFunction(
    classifier: WrapperClassifier,
    feature: ParseNodeFeature
) extends RerankingFunction {

  override def apply(sculpture: Sculpture, baseCost: Double): Double = {
    sculpture match {
      case parse: PolytreeParse =>
        val nodeWeirdness = Range(0, parse.tokens.size) map { tokenIndex =>
          (tokenIndex, classifier.getDistribution(feature(parse, tokenIndex)).getOrElse(0, 0.0))
        }
        val weirdNodes: Set[Int] = ((nodeWeirdness filter { x => x._2 >= 0.5 }) map { _._1 }).toSet
        val weirdDescendants = Range(0, parse.tokens.size).toSet filter { tokenIndex =>
          (parse.paths(tokenIndex).toSet & weirdNodes).nonEmpty
        }
        //val result = (weirdDescendants ++ weirdNodes).size.toDouble
        val result = weirdNodes.size.toDouble
        result
      case _ => Double.MaxValue
    }
  }
}
