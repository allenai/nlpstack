package org.allenai.nlpstack.core.srl

import org.allenai.nlpstack.core.LineProcessor
import org.allenai.nlpstack.core.parse.DependencyParser
import org.allenai.nlpstack.core.parse.graph.DependencyGraph
import org.allenai.nlpstack.core.postag.PostaggedToken

import scala.concurrent.Await
import scala.concurrent.duration._

abstract class Srl {
  def apply(tokens: Seq[PostaggedToken], graph: DependencyGraph): Seq[Frame]
}
