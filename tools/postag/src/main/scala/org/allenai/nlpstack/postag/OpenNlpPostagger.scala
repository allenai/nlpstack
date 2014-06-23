package org.allenai.nlpstack
package postag

import org.allenai.common.Resource
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.nlpstack.tokenize.Token

import opennlp.tools.postag._

import java.net.URL

class OpenNlpPostagger(val model: POSModel) extends Postagger {
  def this() = this(OpenNlpPostagger.loadDefaultModel())

  val postagger = new POSTaggerME(model)

  override def postagTokenized(tokens: Seq[Token]): Seq[PostaggedToken] = {
    val postags = postagger.tag(tokens.iterator.map(_.string).toArray)
    (tokens zip postags).map {
      case (token, postag) =>
        PostaggedToken(token, postag)
    }
  }
}

object OpenNlpPostagger {
  private def defaultModelName = "en-pos-maxent.bin"

  val defaultModelUrl: URL = {
    val url = this.getClass.getClassLoader.getResource(defaultModelName)
    require(url != null, "Could not load default postagger model: " + defaultModelName)
    url
  }

  def loadDefaultModel(): POSModel = {
    Resource.using(defaultModelUrl.openStream()) { stream =>
      new POSModel(stream)
    }
  }
}

object OpenNlpPostaggerMain extends PostaggerMain {
  override val tokenizer = defaultTokenizer
  override val postagger = new OpenNlpPostagger()
}
