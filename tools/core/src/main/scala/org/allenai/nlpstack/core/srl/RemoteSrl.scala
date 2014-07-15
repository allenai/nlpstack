package org.allenai.nlpstack.core.srl

import org.allenai.nlpstack.core.parse.DependencyParser
import org.allenai.nlpstack.core.parse.graph._
import org.allenai.nlpstack.core.postag.PostaggedToken
import org.allenai.nlpstack.core.Remote

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
