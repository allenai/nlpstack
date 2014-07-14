package org.allenai.nlpstack.chunk

import org.allenai.nlpstack.tokenize._
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.parse.DependencyParser
import org.allenai.common.testkit.UnitSpec

class DependencyGraphSpec extends UnitSpec {
  "DependencyGraph" should "round trip through serialization" in {
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

  it should "find the first root if there are multiple" in {
    val sentence = "Alice sings, John said."
    val multilinePickled =
      """|Alice 0 NNP
         |sings 6 VBD
         |, 11 .
         |John 13 NNP
         |said 18 VBD
         |. 22 .
         |
         |nsubj(sings-2, Alice-1)
         |root(ROOT-0, sings-2)
         |nsubj(said-5, John-4)""".stripMargin

    val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(multilinePickled)
    val collapsedGraph = dgraph.collapse

    assert(DependencyParser.multilineStringFormat.write((tokens, collapsedGraph)) === multilinePickled)
  }
}
