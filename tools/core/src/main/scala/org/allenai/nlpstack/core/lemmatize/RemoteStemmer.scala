package org.allenai.nlpstack.core.lemmatize

import org.allenai.nlpstack.core.Remote

import scala.concurrent.ExecutionContext

class RemoteStemmer(val urlString: String)(implicit executionContext: ExecutionContext) extends Stemmer with Remote {
  override def stem(word: String) = {
    post(word)
  }
}
