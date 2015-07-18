package org.allenai.nlpstack.parse

import java.io.File

import org.allenai.datastore.Datastores
import org.allenai.nlpstack.core.DependencyParser
import org.allenai.nlpstack.core.graph.Graph
import org.allenai.nlpstack.core.parse.graph.{ DependencyGraph, DependencyNode }
import org.allenai.nlpstack.core.PostaggedToken
import org.allenai.nlpstack.parse.poly.polyparser
import org.allenai.nlpstack.parse.poly.polyparser.{ MultiPolytreeParseSource, ConllX, FileBasedPolytreeParseSource }

/** Wrapper for the polyparser using the DependencyParser interface.
  *
  * @param modelFile filename for the parser model
  * @param modelVersion version of the parser model
  * @param useLocalFile if false, then the model file is found on the datastore
  */
class PolytreeParser(
    modelFile: String = "PolyParserModel.poly.json",
    modelVersion: Int = 18, useLocalFile: Boolean = false,
    cacheFiles: Iterable[File] = Seq[File]()
) extends DependencyParser with Datastores {

  val parser = {
    val parserConfig =
      if (useLocalFile) {
        modelFile
      } else {
        publicFile(modelFile, modelVersion).toString
      }
    if (cacheFiles.isEmpty) {
      polyparser.Parser.loadParser(parserConfig)
    } else {
      val cachedParses = MultiPolytreeParseSource(cacheFiles map { filename =>
        FileBasedPolytreeParseSource(filename.toString, ConllX(true))
      })
      polyparser.Parser.loadParserWithCache(parserConfig, cachedParses.parseIterator)
    }
  }

  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    // throw away postags
    val parseOption = parser.parseStringSequence(tokens.map(t => t.string))

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
      (childIndex, label) <- arclabels.filter(t => childIndices.contains(t._1))
    ) yield {
      new Graph.Edge(nodes(parentIndex - 1), nodes(childIndex - 1), label.toString.toLowerCase)
    }

    val nodesWithIncomingEdges = edges.map(_.dest).toSet
    val nodesWithoutIncomingEdges = nodes.toSet -- nodesWithIncomingEdges
    require(
      nodesWithoutIncomingEdges.size <= 1,
      s"Parser output for sentence '${tokens.map(_.string).mkString(" ")}' has multiple roots."
    )

    DependencyGraph(nodes.toSet, edges.toSet)
  }
}
