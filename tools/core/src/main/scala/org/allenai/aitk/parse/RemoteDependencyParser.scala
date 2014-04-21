package org.allenai.aitk
package parse

import org.allenai.aitk.chunk._
import org.allenai.aitk.parse.graph._
import org.allenai.aitk.postag._
import org.allenai.aitk.tokenize._

import scala.concurrent.ExecutionContext

class RemoteDependencyParser(val urlString: String)(implicit executionContext: ExecutionContext) extends Remote {
  def dependencyGraph(sentence: String) = {
    val response = post(sentence)

    DependencyParser.multilineStringFormat.read(response)
  }
}
