package org.allenai.nlpstack.core.remote

import org.allenai.nlpstack.core.DependencyParser

import scala.concurrent.ExecutionContext

class RemoteDependencyParser(
    val urlString: String
)(implicit executionContext: ExecutionContext) extends Remote {
  def dependencyGraph(sentence: String) = {
    val response = post(sentence)

    DependencyParser.multilineStringFormat.read(response)
  }
}
