package org.allenai.nlpstack.core.parse

import org.allenai.nlpstack.core.chunk._
import org.allenai.nlpstack.core.parse.graph._
import org.allenai.nlpstack.core.postag._
import org.allenai.nlpstack.core.Remote
import org.allenai.nlpstack.core.tokenize._

import scala.concurrent.ExecutionContext

class RemoteDependencyParser(val urlString: String)(implicit executionContext: ExecutionContext) extends Remote {
  def dependencyGraph(sentence: String) = {
    val response = post(sentence)

    DependencyParser.multilineStringFormat.read(response)
  }
}
