package org.allenai.nlpstack.postag

import org.allenai.common.Resource
import org.allenai.nlpstack.core._

import opennlp.tools.postag.{ POSTaggerME, POSModel }

class OpenNlpPostagger extends Postagger {
  private val postagger = new POSTaggerME(OpenNlpPostagger.model)

  override def postagTokenized(tokens: Seq[Token]): Seq[PostaggedToken] = {
    val postags = postagger.tag(tokens.iterator.map(_.string).toArray)
    (tokens zip postags).map {
      case (token, postag) =>
        val fixedPostag = if (token.string == "-") "HYPH" else postag
        PostaggedToken(token, fixedPostag)
    }
  }
}

object OpenNlpPostagger {
  private val defaultModelName = "en-pos-maxent.bin"
  private val model =
    Resource.using(this.getClass.getClassLoader.getResourceAsStream(defaultModelName)) { is =>
      new POSModel(is)
    }
}
