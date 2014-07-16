package org.allenai.nlpstack.tokenize

import org.allenai.nlpstack.core.Tokenizer
import org.allenai.nlpstack.core.remote.Remote

import scala.concurrent.ExecutionContext

class RemoteTokenizer(val urlString: String)(implicit executionContext: ExecutionContext) extends Tokenizer with Remote {
  def tokenize(sentence: String) = {
    val response = post(sentence)
    Tokenizer.multilineStringFormat.read(response)
  }
}
