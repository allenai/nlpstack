package org.allenai.nlpstack
package chunk

import org.allenai.nlpstack.tokenize._
import org.allenai.nlpstack.postag._

import org.allenai.common.testkit.UnitSpec

class TokenizerImplSpec extends TokenizerSpec {
  val tokenizerToTest = new SimpleEnglishTokenizer
}
