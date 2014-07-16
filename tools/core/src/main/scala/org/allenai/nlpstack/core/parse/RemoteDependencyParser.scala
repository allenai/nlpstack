package org.allenai.nlpstack.core.parse

import org.allenai.nlpstack.core.Remote

import scala.concurrent.ExecutionContext

class RemoteDependencyParser(val urlString: String)(implicit executionContext: ExecutionContext) extends Remote {
  def dependencyGraph(sentence: String) = {
    val response = post(sentence)

    DependencyParser.multilineStringFormat.read(response)
  }
}
