package org.allenai.nlpstack.core.parse

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.parse.graph.DependencyNode

class DependencyNodeSpec extends UnitSpec {
  "DependencyNode" should "round trip through string serialization when it contains a hyphen" in {
    val pickledDepNode = "Co-Redemptrix-13"
    val depNode = DependencyNode.stringFormat.read(pickledDepNode)
    val repickled = DependencyNode.stringFormat.write(depNode)

    assert(pickledDepNode === repickled)
  }

  "DependencyNode" should "round trip through json serialization" in {
    val node = new DependencyNode(4, "Michael")
    val pickled = DependencyNode.dependencyNodeJsonFormat.write(node)
    val unpickled = DependencyNode.dependencyNodeJsonFormat.read(pickled)

    assert(node === unpickled)
  }
}

