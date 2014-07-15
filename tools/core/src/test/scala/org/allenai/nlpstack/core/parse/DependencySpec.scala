package org.allenai.nlpstack.core.parse

import org.allenai.nlpstack.core.parse.graph.Dependency
import org.allenai.common.testkit.UnitSpec

class DependencySpec extends UnitSpec {
  "Dependency" should "round trip through serialization" in {
    val pickledDep = "det(reflection-9, the-6)"
    val dep = Dependency.stringFormat.read(pickledDep)
    val repickled = Dependency.stringFormat.write(dep)

    assert(pickledDep === repickled)
  }
}
