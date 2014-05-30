/*
package org.allenai.nlpstack
package parse

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.tokenize._

class ClearParserSpec extends UnitSpec {
  "parser impl" should "work correctly on a single-word sentence" in {
    val text = "John"
    val tokenizer = new SimpleEnglishTokenizer()
    val postagger = new OpenNlpPostagger()
    val parser = new ClearParser()

    val dgraph = parser.dependencyGraph(tokenizer, postagger)(text)
    DependencyParser.multilineStringFormat.write(dgraph) === "John 0 NNP\n\nroot(ROOT-0, John-1)"
  }
}
*/
