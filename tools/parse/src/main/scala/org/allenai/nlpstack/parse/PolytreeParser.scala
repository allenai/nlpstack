package org.allenai.nlpstack.parse

import org.allenai.common.Resource.using
import org.allenai.nlpstack.core.DependencyParser
import org.allenai.nlpstack.core.graph.Graph
import org.allenai.nlpstack.core.parse.graph.{ DependencyGraph, DependencyNode }
import org.allenai.nlpstack.core.PostaggedToken
import org.allenai.parsers.polyparser
import org.allenai.parsers.polyparser.{ NexusToken, WordClusters }

class PolytreeParser extends DependencyParser {
  private val parser =
    new polyparser.GreedyTransitionParser(
      using(
        Thread.currentThread.getContextClassLoader.getResourceAsStream(
          "org/allenai/polyparser-models/wsj.train.30.dstan3_4.dt.poly.json.gz")) {
          polyparser.ClassifierBasedCostFunction.loadFromStream(_)
        })

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
      (token, index) <- parse.tokens.drop(1).zipWithIndex // dropping the nexus token
    ) yield {
      DependencyNode(index, token.word.name)
    }

    val edges = for (
      parse <- parseOption.toList;
      ((arclabels, childIndices), parentIndex) <- (parse.arclabels zip parse.children).zipWithIndex;
      if parentIndex > 0;
      (childIndex, Symbol(label)) <- arclabels.filter(t => childIndices.contains(t._1))
    ) yield {
      new Graph.Edge(nodes(parentIndex - 1), nodes(childIndex - 1), label.toLowerCase)
    }

    val nodesWithIncomingEdges = edges.map(_.dest).toSet
    val nodesWithoutIncomingEdges = nodes.toSet -- nodesWithIncomingEdges
    require(nodesWithoutIncomingEdges.size <= 1, s"Parser output for sentence '${tokens.map(_.string).mkString(" ")}' has multiple roots.")

    DependencyGraph(nodes.toSet, edges.toSet)
  }
}
