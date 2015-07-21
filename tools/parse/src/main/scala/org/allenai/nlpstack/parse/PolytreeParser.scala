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
  * This API allows you to optionally specify a set of files (in Conll-X format) containing
  * "gold" parses. If such parses are specified, then the parser will populate a cache with these
  * parses. Then whenever it is asked to parse a sentence, it will check this cache first, and
  * only parse a sentence from scratch if there is a cache miss.
  *
  * @param modelFile filename for the parser model
  * @param modelVersion version of the parser model
  * @param useLocalFile if false, then the model file is found on the datastore
  * @param cacheFiles a sequence of CoNLL files containing "gold" parses to cache
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
        FileBasedPolytreeParseSource(filename.toString, ConllX(true, makePoly = true))
      })
      polyparser.Parser.loadParserWithCache(parserConfig, cachedParses.parseIterator)
    }
  }

  /** Do a simultaneously postagging and parsing using the PolytreeParser.
    *
    * @param tokens the words of the sentence you want to parse
    * @return the parse tree and the POS tags of the sentence words
    */
  def postagAndParse(tokens: Seq[String]): (DependencyGraph, Seq[String]) = {
    val parseOption = parser.parseStringSequence(tokens)
    val nodes = for (
      parse <- parseOption.toList;
      (token, index) <- parse.tokens.drop(1).zipWithIndex // dropping the nexus token
    ) yield {
      DependencyNode(index, token.word.name)
    }
    val edges = for (
      parse <- parseOption.toList;
      ((arclabels, childIndices), parentIndex) <- (parse.arclabels zip parse.children).zipWithIndex if parentIndex > 0;
      (childIndex, label) <- arclabels.filter(t => childIndices.contains(t._1))
    ) yield {
      new Graph.Edge(nodes(parentIndex - 1), nodes(childIndex - 1), label.toString.toLowerCase)
    }
    val nodesWithIncomingEdges = edges.map(_.dest).toSet
    val nodesWithoutIncomingEdges = nodes.toSet -- nodesWithIncomingEdges
    require(
      nodesWithoutIncomingEdges.size <= 1,
      s"Parser output for sentence '${tokens.mkString(" ")}' has multiple roots."
    )
    (DependencyGraph(nodes.toSet, edges.toSet), parseOption.get.sentence.tokens.tail map { x =>
      x.getDeterministicProperty('cpos).name
    })
  }

  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    val (result, _) = postagAndParse(tokens.map(t => t.string)) // throw away postags
    result
  }
}
