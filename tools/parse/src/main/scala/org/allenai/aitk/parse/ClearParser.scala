package org.allenai.aitk
package parse

import com.clearnlp.dependency.DEPNode
import com.clearnlp.dependency.DEPTree
import com.clearnlp.nlp.NLPGetter
import com.clearnlp.nlp.NLPMode

import org.allenai.aitk.parse.graph.Dependency
import org.allenai.aitk.parse.graph.DependencyGraph
import org.allenai.aitk.parse.graph.DependencyNode
import org.allenai.aitk.postag._
import org.allenai.aitk.tokenize._
import org.allenai.common.Resource.using

import java.io.PrintWriter
import java.lang.ProcessBuilder
import java.util.zip.ZipInputStream

import scala.collection.JavaConverters._

class ClearParser(
    val tokenizer: Tokenizer = new SimpleEnglishTokenizer,
    val postagger: Postagger = new OpenNlpPostagger()) extends DependencyParser {
  val clearMorpha = NLPGetter.getComponent("general-en", "en", NLPMode.MODE_MORPH)

  val clearDepParser = NLPGetter.getComponent("general-en", "en", NLPMode.MODE_DEP)

  def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    val tree = new DEPTree()
    tokens.zipWithIndex.foreach {
      case (token, i) =>
        val node = new DEPNode(i + 1, token.string)
        node.pos = token.postag
        tree.add(node)
    }

    clearMorpha.process(tree)
    clearDepParser.process(tree)

    ClearParser.graphFromTree(tree, tokens)
  }
}

object ClearParser {
  def graphFromTree(tree: DEPTree, tokens: Seq[Token]): DependencyGraph = {
    val nodeMap = (for ((node, i) <- tree.iterator.asScala.zipWithIndex) yield {
      node.id -> new DependencyNode(i - 1, node.form)
    }).toMap

    val deps = for {
      sourceNode <- tree.iterator.asScala.toList
      if sourceNode.hasHead
      if sourceNode.id != 0
      label = sourceNode.getLabel
      destNode = sourceNode.getHead
      if destNode.id != 0
    } yield {
      new Dependency(nodeMap(destNode.id), nodeMap(sourceNode.id), label)
    }

    DependencyGraph(nodeMap.values.toSet filterNot (_.id == -1), deps.toSet)
  }
}

object ClearDependencyParserMain extends DependencyParserMain {
  lazy val dependencyParser = new ClearParser()
}
