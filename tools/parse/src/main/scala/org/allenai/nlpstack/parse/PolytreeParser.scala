package org.allenai.nlpstack.parse

import org.allenai.nlpstack.graph.Graph
import org.allenai.nlpstack.parse.graph.{ DependencyNode, DependencyGraph }
import org.allenai.nlpstack.postag.{ PostaggedToken, defaultPostagger }
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.parsers.polyparser
import org.allenai.parsers.polyparser.{ NexusToken, WordClusters, Transition }

class PolytreeParser extends DependencyParser {
  private val parser =
    new polyparser.GreedyTransitionParser(
      polyparser.ClassifierBasedCostFunction.loadFromClasspath(
        "org/allenai/polyparser-models/example1.poly.json.gz"))

  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]) = {
    val polyTokens = for (token <- tokens) yield {
      polyparser.Token(
        Symbol(token.string),
        Some(Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(token.postag, token.postag))),
        Some(token.postagSymbol))
    }
    val polyTokensVector = NexusToken +: polyTokens.toVector
      // Polyparser needs a nexus token in its initial state.
    val initialState =
      polyparser.TransitionParserState.initialState(polyTokensVector)
    val transitionsOption = parser.parse(initialState)
    val parseOption = transitionsOption.map(
      polyparser.PolytreeParse.fromTransitions(polyTokensVector, _))

    val nodes = for (
      parse <- parseOption.toList;
      (token, index) <- parse.tokens.drop(1).zipWithIndex   // dropping the nexus token
    ) yield {
      DependencyNode(index, token.word.name)
    }

    val edges = for (
      parse <- parseOption.toList;
      ((arclabels, childIndices), parentIndex) <- (parse.arclabels zip parse.children).zipWithIndex;
      if parentIndex > 0;
      (childIndex, Symbol(label)) <- arclabels.filter(t => childIndices.contains(t._1))
    ) yield {
      new Graph.Edge(nodes(parentIndex - 1), nodes(childIndex - 1), label)
    }

    val nodesWithIncomingEdges = edges.map(_.dest).toSet
    val nodesWithoutIncomingEdges = nodes.toSet -- nodesWithIncomingEdges
    val firstRoot = nodesWithoutIncomingEdges.toSeq(0)

    DependencyGraph(Some(firstRoot), nodes.toSet, edges.toSet)
  }
}

object PolytreeParserMain extends DependencyParserMain {
  override lazy val tokenizer = defaultTokenizer
  override lazy val postagger = defaultPostagger
  override lazy val dependencyParser = new PolytreeParser
}
