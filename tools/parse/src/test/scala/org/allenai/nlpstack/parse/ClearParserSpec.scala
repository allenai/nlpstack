package org.allenai.nlpstack
package parse

import org.allenai.common.testkit.UnitSpec

class ClearParserSpec extends UnitSpec {
  "parser impl" should "work correctly on a single-word sentence" in {
    val text = "John"
    val parser = new ClearParser()

    val dgraph = parser.dependencyGraph(text)
    DependencyParser.multilineStringFormat.write(dgraph) === "John 0 NNP\n\nroot(ROOT-0, John-1)"
  }
}
