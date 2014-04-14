package org.allenai.aitk
package srl

import org.allenai.aitk.parse.graph._
import org.allenai.aitk.postag.PostaggedToken
import scala.concurrent.ExecutionContext
import org.allenai.aitk.parse.DependencyParser

class RemoteSrl(val urlString: String)(implicit executionContext: ExecutionContext) extends Srl with Remote {
  def apply(tokens: Seq[PostaggedToken], dgraph: DependencyGraph) = {
    val response = this.post(DependencyParser.multilineStringFormat.write(tokens -> dgraph))
    if (response.isEmpty) Seq.empty
    else {
      response.split("\\n").map(Frame.deserialize(dgraph))(scala.collection.breakOut)
    }
  }
}
