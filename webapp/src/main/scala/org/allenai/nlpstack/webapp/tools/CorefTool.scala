package org.allenai.nlpstack.webapp.tools

import org.allenai.nlpstack.core.coref.Referent
import org.allenai.nlpstack.core.coref.CorefResolver
import org.allenai.nlpstack.core.parse.graph.DependencyGraph
import org.allenai.nlpstack.parse.FactorieParser

object CorefTool extends Tool("coref") with StringFormat {
  type Output = (DependencyGraph, Seq[Referent])

  private val parser = new FactorieParser
  // The Factorie coref tool is not currently not reliable with MarkH's
  // polyparser.

  override def info =
    ToolInfo(
      Impl.coref.getClass.getSimpleName,
      "Our fake plants died because we did not pretend to water them."
    )
  override def split(input: String) = input split "\n"
  override def process(section: String) = {
    val tokens = Impl.tokenizer(section)
    val postags = Impl.postagger.postagTokenized(tokens)
    val dgraph = parser.dependencyGraphPostagged(postags)
    (dgraph, Impl.coref.resolveCoreferences((postags, dgraph)))
  }
  override def visualize(output: Output) = Seq.empty

  override def stringFormat = CorefResolver.multilineStringFormat
}
