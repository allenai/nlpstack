package org.allenai.nlpstack.srl

import org.allenai.nlpstack.parse.DependencyParser
import org.allenai.nlpstack.parse.graph._
import org.allenai.nlpstack.postag.PostaggedToken
import org.allenai.nlpstack.Remote

import scala.concurrent.ExecutionContext

class RemoteSrl(val urlString: String)(implicit executionContext: ExecutionContext) extends Srl with Remote {
  def apply(tokens: Seq[PostaggedToken], dgraph: DependencyGraph) = {
    val response = this.post(DependencyParser.multilineStringFormat.write(tokens -> dgraph))
    if (response.isEmpty) Seq.empty
    else {
      response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
    }
  }
}
