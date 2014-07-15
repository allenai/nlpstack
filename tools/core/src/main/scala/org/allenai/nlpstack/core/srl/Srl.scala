package org.allenai.nlpstack.core.srl

import org.allenai.nlpstack.core.parse.graph.DependencyGraph
import org.allenai.nlpstack.core.postag.PostaggedToken

abstract class Srl {
  def apply(tokens: Seq[PostaggedToken], graph: DependencyGraph): Seq[Frame]
}
