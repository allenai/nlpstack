package org.allenai.nlpstack
package postag

import org.allenai.nlpstack.core.postag.PostaggerMain
import org.allenai.nlpstack.tokenize.defaultTokenizer

@deprecated("Please use FactoriePostagger instead", "2014-06-23")
class OpenNlpPostagger extends FactoriePostagger

object OpenNlpPostaggerMain extends PostaggerMain {
  override val tokenizer = defaultTokenizer
  override val postagger = new OpenNlpPostagger()
}
