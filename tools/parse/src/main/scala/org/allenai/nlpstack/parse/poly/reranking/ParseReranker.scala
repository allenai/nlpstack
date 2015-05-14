package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.fsm.RerankingFunction
import org.allenai.nlpstack.parse.poly.polyparser._

import java.io._

import scopt.OptionParser

case class ParseRerankerCommandLine(
  parserFilename: String = "",
  goldParseFilename: String = "",
  dataSource: String = "",
  rerankerFilename: String = "",
  diagnosticFilename: String = ""
)

/** This command-line takes a serialized reranking function and uses it to rerank an n-best
  * list. The resulting parses are then evaluated against a gold set.
  */
object ParseReranker {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[ParseRerankerCommandLine]("ParseRerankerTraining") {
      opt[String]('p', "parser") required () valueName "<file>" action { (x, c) =>
        c.copy(parserFilename = x)
      } text "the file containing the nbest lists"
      opt[String]('g', "goldfile") required () valueName "<file>" action { (x, c) =>
        c.copy(goldParseFilename = x)
      } text "the file containing the gold parses"
      opt[String]('r', "reranker") required () valueName "<file>" action { (x, c) =>
        c.copy(rerankerFilename = x)
      } text "the file containing the serialized reranking function"
      opt[String]('l', "diagnosticfile") required () valueName "<string>" action { (x, c) =>
        c.copy(diagnosticFilename = x)
      } text "where to write the reranking function"
      opt[String]('d', "datasource") required () valueName "<file>" action { (x, c) =>
        c.copy(dataSource = x)
      } text "the location of the data ('datastore','local')" validate { x =>
        if (Set("datastore", "local").contains(x)) {
          success
        } else {
          failure(s"unsupported data source: $x")
        }
      }
    }
    val clArgs: ParseRerankerCommandLine =
      optionParser.parse(args, ParseRerankerCommandLine()).get
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val parser: TransitionParser = TransitionParser.load(clArgs.parserFilename)
    val reranker = RerankingFunction.load(clArgs.rerankerFilename)

    val diagnosticWriter = new PrintWriter(new File(clArgs.diagnosticFilename))

    println("Parsing test set.")
    val candidateParses: Iterable[Option[PolytreeParse]] = {
      ParseFile.parseTestSet(parser, goldParseSource).toIterable
    }

    println("Evaluating test set.")

    reranker match {
      case weirdReranker: WeirdParseNodeRerankingFunction =>
        val analyzer = WeirdnessAnalyzer(weirdReranker)
        for {
          (candidateParse, goldParse) <- candidateParses.iterator.zip(goldParseSource.parseIterator)
        } analyzer.notify(candidateParse, goldParse)
        analyzer.report(Some(diagnosticWriter))
    }

    diagnosticWriter.close()
  }
}

/** A parse statistic that collects weirdness statistics for candidate parses.
  *
  * @param rerankingFunction the weirdness reranking function
  */
case class WeirdnessAnalyzer(rerankingFunction: WeirdParseNodeRerankingFunction) {

  var sentenceGoldParsesAndMistakes = Seq[(PolytreeParse, Set[String], Set[String])]()

  def reset(): Unit = {
    sentenceGoldParsesAndMistakes = Seq[(PolytreeParse, Set[String], Set[String])]()
  }

  def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    candidateParse map { candParse =>
      val weirdGoldNodes = rerankingFunction.getNodesWithOutcome(goldParse, 1)
      val notWeirdCandidateNodes = rerankingFunction.getNodesWithOutcome(candParse, 0)
      val badCandidateTokens: Set[Int] =
        (candParse.families.toSet -- goldParse.families.toSet) map {
          case family =>
            family.tokens.head
        } filter { tokIndex =>
          candParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }

      val falsePositiveMessages = weirdGoldNodes flatMap {
        case (goldNodeIx, Some(justification)) =>
          Some(s"  ${goldParse.printFamily(goldNodeIx)}\nClassifier Explanation: " +
            s"${justification.prettyPrint(rerankingFunction.classifier.featureNameMap.toMap)}\n")
        case _ => None
      }
      val trueNegativeNodesAndJustifications = notWeirdCandidateNodes filter {
        node => badCandidateTokens.contains(node._1)
      }
      val trueNegativeMessages = trueNegativeNodesAndJustifications flatMap {
        case (misclassifiedNodeIx, Some(justification)) =>
          Some(s"  ${candParse.printFamily(misclassifiedNodeIx)}\n" +
            s"    (should be: ${goldParse.printFamily(misclassifiedNodeIx)})" +
            s"\nClassifier Explanation:" +
            s" ${justification.prettyPrint(rerankingFunction.classifier.featureNameMap.toMap)}\n")
        case _ =>
          None
      }

      if (falsePositiveMessages.nonEmpty || trueNegativeMessages.nonEmpty) {
        sentenceGoldParsesAndMistakes = sentenceGoldParsesAndMistakes :+
          Tuple3(goldParse, falsePositiveMessages, trueNegativeMessages)
      }
    }
  }

  def report(diagnosticWriter: Option[PrintWriter]): Unit = {
    def logMessage(statWriter: Option[PrintWriter], message: String): Unit = {
      statWriter match {
        case Some(w) => w.println(message)
        case None => println(message)
      }
    }
    sentenceGoldParsesAndMistakes foreach {
      case (goldParse, falsePositiveMessages, trueNegativeMessages) =>
        logMessage(diagnosticWriter, "")
        logMessage(diagnosticWriter, goldParse.sentence.asWhitespaceSeparatedString)
        logMessage(diagnosticWriter, "Gold Families:")
        val goldFamilyStrings = (Range(0, goldParse.tokens.size) map {
          tokenIx => goldParse.printFamily(tokenIx)
        }).toSet
        goldFamilyStrings.foreach { goldParseFamily =>
          logMessage(diagnosticWriter, s"  $goldParseFamily")
        }
        logMessage(diagnosticWriter, "")
        if (falsePositiveMessages.nonEmpty) {
          logMessage(diagnosticWriter, "Good families, classified as weird:")
          falsePositiveMessages foreach { message =>
            logMessage(diagnosticWriter, message)
          }
        }
        if (trueNegativeMessages.nonEmpty) {
          logMessage(diagnosticWriter, "Bad families, classified as good:")
          trueNegativeMessages foreach { message =>
            logMessage(diagnosticWriter, message)
          }
        }
    }
  }
}

