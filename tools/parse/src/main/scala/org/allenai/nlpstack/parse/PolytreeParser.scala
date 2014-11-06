package org.allenai.nlpstack.parse

import org.allenai.common.Resource.using
import org.allenai.nlpstack.core.DependencyParser
import org.allenai.nlpstack.core.graph.Graph
import org.allenai.nlpstack.core.parse.graph.{ DependencyGraph, DependencyNode }
import org.allenai.nlpstack.core.PostaggedToken
import org.allenai.parsers.polyparser
import org.allenai.parsers.polyparser.{ Parser, NexusToken }

class PolytreeParser extends DependencyParser {
  private val parser =
    using(
      this.getClass.getClassLoader.getResourceAsStream(
        "org/allenai/polyparser-models/ThomasJefferson.poly.json"
      )
    ) {
        Parser.loadParser(_)
      }

  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]) = {
    val polyTokens = tokens.map(t => polyparser.Token.create(t.string, t.postag))
    val parseOption = parser.parse(polyparser.Sentence(NexusToken +: polyTokens))

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
