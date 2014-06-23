package org.allenai.nlpstack
package postag

import org.allenai.nlpstack.tokenize._
import org.allenai.common.testkit.UnitSpec

class OpenNlpPostaggerSpec extends PostaggerSpec {
  val taggerToTest = new OpenNlpPostagger
}

