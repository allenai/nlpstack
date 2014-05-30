package org.allenai.nlpstack
package parse

import org.allenai.nlpstack.chunk._
import org.allenai.nlpstack.parse.graph._
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.tokenize._

import scala.concurrent.ExecutionContext

class RemoteDependencyParser(val urlString: String)(implicit executionContext: ExecutionContext) extends Remote {
  def dependencyGraph(sentence: String) = {
    val response = post(sentence)

    DependencyParser.multilineStringFormat.read(response)
  }
}
