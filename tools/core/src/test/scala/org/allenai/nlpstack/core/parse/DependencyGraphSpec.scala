package org.allenai.nlpstack.core.parse

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.parse.graph.DependencyGraph

class DependencyGraphSpec extends UnitSpec {
  "DependencyGraph" should "round trip through string serialization" in {
    val sentence = "John very quickly ran away from the deep blue reflection in the mirror."

    val multilinePickled = """John 0 NNP
very 5 RB
quickly 10 RB
ran 18 VBD
away 22 RB
from 27 IN
the 32 DT
deep 36 JJ
blue 41 JJ
reflection 46 NN
in 57 IN
the 60 DT
mirror 64 NN
. 70 .

nsubj(ran-4, John-1)
advmod(quickly-3, very-2)
advmod(ran-4, quickly-3)
root(ROOT-0, ran-4)
advmod(ran-4, away-5)
prep(ran-4, from-6)
det(reflection-10, the-7)
amod(reflection-10, deep-8)
amod(reflection-10, blue-9)
pobj(from-6, reflection-10)
prep(reflection-10, in-11)
det(mirror-13, the-12)
pobj(in-11, mirror-13)"""

    // deserialize and check counts
    val dgraph @ (tokens, graph) = DependencyParser.multilineStringFormat.read(multilinePickled)
    assert(tokens.size === 14)

    // reserialize and check match
    val repickled = DependencyParser.multilineStringFormat.write(dgraph)
    assert(multilinePickled === repickled)
  }

  it should "round trip through json serialization" in {
    val pickled = """John 0 NNP
very 5 RB
quickly 10 RB
ran 18 VBD
away 22 RB

nsubj(ran-4, John-1)
advmod(quickly-3, very-2)
advmod(ran-4, quickly-3)
root(ROOT-0, ran-4)
advmod(ran-4, away-5)"""

    val dgraph @ (tokens, graph) = DependencyParser.multilineStringFormat.read(pickled)

    val json = DependencyGraph.dependencyGraphJsonFormat.write(graph)
    assert(DependencyGraph.dependencyGraphJsonFormat.read(json) === graph)
  }
}
