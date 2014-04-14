package org.allenai.aitk
package parse

import org.allenai.aitk.chunk._
import org.allenai.aitk.parse.graph._
import org.allenai.aitk.postag._
import org.allenai.aitk.tokenize._

import scala.concurrent.ExecutionContext

class RemoteDependencyParser(val urlString: String)(implicit executionContext: ExecutionContext) extends DependencyParser with Remote {
  override def tokenizer = throw new UnsupportedOperationException()
  override def postagger = throw new UnsupportedOperationException()

  override def dependencyGraph(sentence: String) = {
    val response = post(sentence)

    DependencyParser.multilineStringFormat.read(response)
  }

  /**
    * Throws UnsupportedOperationException
    */
  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]): DependencyGraph = {
    throw new UnsupportedOperationException()
  }

  /**
    * Throws UnsupportedOperationException
    */
  override def dependencyGraphTokenized(tokens: Seq[Token]): (Seq[PostaggedToken], DependencyGraph) = {
    throw new UnsupportedOperationException()
  }
}
