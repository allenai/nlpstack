package org.allenai
package nlpstack
package parse

import org.allenai.nlpstack.parse.graph.DependencyNode
import org.allenai.common.testkit.UnitSpec

class DependencyNodeSpec extends UnitSpec {
  "DependencyNode" should "round trip through serialization when it contains a hyphen" in {
    val pickledDepNode = "Co-Redemptrix-13"
    val depNode = DependencyNode.stringFormat.read(pickledDepNode)
    val repickled = DependencyNode.stringFormat.write(depNode)

    assert(pickledDepNode === repickled)
  }
}

