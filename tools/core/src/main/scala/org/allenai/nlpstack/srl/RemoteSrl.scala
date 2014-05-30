package org.allenai.nlpstack
package srl

import org.allenai.nlpstack.parse.graph._
import org.allenai.nlpstack.postag.PostaggedToken
import scala.concurrent.ExecutionContext
import org.allenai.nlpstack.parse.DependencyParser

class RemoteSrl(val urlString: String)(implicit executionContext: ExecutionContext) extends Srl with Remote {
  def apply(tokens: Seq[PostaggedToken], dgraph: DependencyGraph) = {
    val response = this.post(DependencyParser.multilineStringFormat.write(tokens -> dgraph))
    if (response.isEmpty) Seq.empty
    else {
      response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
    }
  }
}
