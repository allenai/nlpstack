package org.allenai.nlpstack.core.parse

import org.allenai.nlpstack.core.LineProcessor
import org.allenai.nlpstack.core.postag.Postagger
import org.allenai.nlpstack.core.tokenize._

abstract class DependencyParserMain extends LineProcessor("dep-parser") {
  def tokenizer: Tokenizer
  def postagger: Postagger
  def dependencyParser: DependencyParser

  override def init(config: Config) {
    // for timing purposes
    val tokens = tokenizer("I want to initialize the parser.")
    val postagged = postagger.postagTokenized(tokens)
    dependencyParser.dependencyGraphPostagged(postagged)
  }

  override def process(line: String) = {
    val tokens = tokenizer(line)
    val postagged = postagger.postagTokenized(tokens)
    val dgraph = dependencyParser.dependencyGraphPostagged(postagged)

    // produce connl format
    val builder = new StringBuilder
    for ((token, i) <- postagged.zipWithIndex) {
      builder ++= token.string
      builder ++= "\t\t\t"
      builder ++= token.postag
      builder ++= "\t\t"
      val node = dgraph.nodes.find(_.id == i).get
      val parentEdges = dgraph.incoming(node)
      if (parentEdges.isEmpty) {
        builder ++= "0\tROOT"
      } else {
        assert(parentEdges.size == 1)
        val parentEdge = parentEdges.head
        val parent = parentEdge.source
        builder ++= parent.id.toString
        builder ++= "\t"
        builder ++= parentEdge.label
      }
      builder ++= "\t\t\n"
    }

    // cut off trailing \n
    builder.length = builder.length - 1
    builder.result
  }
}
