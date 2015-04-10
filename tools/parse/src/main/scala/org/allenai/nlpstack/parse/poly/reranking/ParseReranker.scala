package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.eval._
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
    val candidateParses: Iterator[Option[PolytreeParse]] = {
      goldParseSource.parseIterator map {
        parse => parser.parse(parse.sentence)
      }
    }
    reranker match {
      case weirdReranker: WeirdParseNodeRerankingFunction =>
        val stats: Seq[ParseStatistic] = Seq(WeirdnessAnalyzer(weirdReranker))
        stats foreach { stat => stat.reset() }
        ParseEvaluator.evaluate(
          candidateParses, goldParseSource.parseIterator, stats, Some(diagnosticWriter)
        )
    }
    diagnosticWriter.close
  }
}

/** A parse statistic that collects weirdness statistics for candidate parses.
  *
  * @param rerankingFunction the weirdness reranking function
  */
case class WeirdnessAnalyzer(rerankingFunction: WeirdParseNodeRerankingFunction)
    extends ParseStatistic {

  var sentencesWithMistakes = Seq[(Sentence, Set[String], Set[String])]()

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    candidateParse map { candParse =>
      val weirdGoldNodes = rerankingFunction.getWeirdNodes(goldParse)
      val notWeirdCandidateNodes = rerankingFunction.getNotWeirdNodes(candParse)
      val badCandidateTokens: Set[Int] =
        (candParse.families.toSet -- goldParse.families.toSet) map {
          case family =>
            family.tokens.head
        } filter { tokIndex =>
          candParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }

      val falsePositiveMessages = weirdGoldNodes map {
        case (goldNodeIx, justification) =>
          s"  ${goldParse.printFamily(goldNodeIx)}" +
            s"\nClassifier Justification: ${justification.getOrElse("")}\n"
      }
      val trueNegativeNodesAndJustifications = notWeirdCandidateNodes filter {
        node => badCandidateTokens.contains(node._1)
      }
      val trueNegativeMessages = trueNegativeNodesAndJustifications map {
        case (misclassifiedNodeIx, justification) =>
          s"  ${candParse.printFamily(misclassifiedNodeIx)}\n" +
            s"    (should be: ${goldParse.printFamily(misclassifiedNodeIx)})" +
            s" \nClassifier Justification: ${justification.getOrElse("")}\n"
      }

      if (falsePositiveMessages.nonEmpty || trueNegativeMessages.nonEmpty) {
        sentencesWithMistakes = sentencesWithMistakes :+
          Tuple3(goldParse.sentence, falsePositiveMessages, trueNegativeMessages)
      }
    }
  }

  override def report(diagnosticWriter: Option[PrintWriter]): Unit = {
    sentencesWithMistakes foreach {
      case (sentence, falsePositiveMessages, trueNegativeMessages) =>
        logMessage(diagnosticWriter, "")
        logMessage(diagnosticWriter, sentence.asWhitespaceSeparatedString)
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

  override def reset(): Unit = {
    sentencesWithMistakes = Seq[(Sentence, Set[String], Set[String])]()
  }
}
