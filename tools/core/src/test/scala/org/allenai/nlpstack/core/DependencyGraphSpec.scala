package org.allenai.nlpstack.core

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

  private def graphIsValid(graph: DependencyGraph) =
    graph.vertices.forall(_.id >= 0)

  it should "collapse a simple sentence" in {
    val uncollapsedString =
      """|det(waffle-2, A-1)
         |nsubj(is-3, waffle-2)
         |root(ROOT-0, is-3)
         |prep(is-3, like-4)
         |det(pancake-6, a-5)
         |pobj(like-4, pancake-6)
         |prep(pancake-6, with-7)
         |det(trap-10, a-8)
         |nn(trap-10, syrup-9)
         |pobj(with-7, trap-10)
         |punct(is-3, .-11)""".stripMargin
    val uncollapsed = DependencyGraph.multilineStringFormat.read(uncollapsedString)

    val expectedCollapsedString =
      """|det(waffle-2, A-1)
         |nsubj(is-3, waffle-2)
         |root(ROOT-0, is-3)
         |det(pancake-6, a-5)
         |prep_like(is-3, pancake-6)
         |det(trap-10, a-8)
         |nn(trap-10, syrup-9)
         |prep_with(pancake-6, trap-10)
         |punct(is-3, .-11)""".stripMargin
    assert(DependencyGraph.multilineStringFormat.write(uncollapsed.collapse) === expectedCollapsedString)
    assert(graphIsValid(uncollapsed.collapse))
  }

  it should "collapse a sentence with a broken multiword preposition" in {
    val uncollapsedString =
      """|det(amphibians-3, The-1)
         |amod(amphibians-3, first-2)
         |nn(.-25, amphibians-3)
         |vmod(amphibians-3, evolved-4)
         |aux(move-6, to-5)
         |xcomp(evolved-4, move-6)
         |prep(move-6, out-7)
         |pcomp(out-7, of-8)
         |nsubj(.-25, the-9)
         |nn(.-25, water-10)
         |cc(water-10, and-11)
         |nn(land-13, colonize-12)
         |conj(water-10, land-13)
         |punct(water-10, ,-14)
         |cc(water-10, but-15)
         |nsubj(had-17, they-16)
         |rcmod(water-10, had-17)
         |aux(return-19, to-18)
         |xcomp(had-17, return-19)
         |prep(return-19, to-20)
         |det(water-22, the-21)
         |pobj(to-20, water-22)
         |aux(reproduce-24, to-23)
         |vmod(water-22, reproduce-24)
         |root(ROOT-0, .-25)""".stripMargin
    val uncollapsed = DependencyGraph.multilineStringFormat.read(uncollapsedString)

    val expectedCollapsedString =
      """|det(amphibians-3, The-1)
         |amod(amphibians-3, first-2)
         |nn(.-25, amphibians-3)
         |vmod(amphibians-3, evolved-4)
         |aux(move-6, to-5)
         |xcomp(evolved-4, move-6)
         |nsubj(.-25, the-9)
         |nn(.-25, water-10)
         |nn(land-13, colonize-12)
         |conj_but(water-10, land-13)
         |punct(water-10, ,-14)
         |nsubj(had-17, they-16)
         |rcmod(water-10, had-17)
         |aux(return-19, to-18)
         |xcomp(had-17, return-19)
         |det(water-22, the-21)
         |prep_to(return-19, water-22)
         |aux(reproduce-24, to-23)
         |vmod(water-22, reproduce-24)
         |root(ROOT-0, .-25)""".stripMargin
    assert(DependencyGraph.multilineStringFormat.write(uncollapsed.collapse) === expectedCollapsedString)
    assert(graphIsValid(uncollapsed.collapse))
  }

  it should "collapse a sentence with a conjunction that has only one argument" in {
    val uncollapsedString =
      """|amod(mouth-3, Covering-1)
         |poss(mouth-3, your-2)
         |nn(.-21, mouth-3)
         |advmod(cough-6, when-4)
         |nsubj(cough-6, you-5)
         |rcmod(mouth-3, cough-6)
         |cc(cough-6, or-7)
         |nsubj(way-11, sneeze-8)
         |cop(way-11, is-9)
         |det(way-11, another-10)
         |rcmod(or-7, way-11)
         |aux(prevent-13, to-12)
         |vmod(way-11, prevent-13)
         |poss(germs-15, your-14)
         |dobj(prevent-13, germs-15)
         |prep(prevent-13, from-16)
         |pcomp(from-16, passing-17)
         |prep(passing-17, to-18)
         |pobj(to-18, someone-19)
         |advmod(passing-17, else-20)
         |root(ROOT-0, .-21)""".stripMargin
    val uncollapsed = DependencyGraph.multilineStringFormat.read(uncollapsedString)

    val expectedCollapsedString =
      """|amod(mouth-3, Covering-1)
         |poss(mouth-3, your-2)
         |nn(.-21, mouth-3)
         |advmod(cough-6, when-4)
         |nsubj(cough-6, you-5)
         |rcmod(mouth-3, cough-6)
         |cc(cough-6, or-7)
         |nsubj(way-11, sneeze-8)
         |cop(way-11, is-9)
         |det(way-11, another-10)
         |rcmod(or-7, way-11)
         |aux(prevent-13, to-12)
         |vmod(way-11, prevent-13)
         |poss(germs-15, your-14)
         |dobj(prevent-13, germs-15)
         |prepc_from(prevent-13, passing-17)
         |prep_to(passing-17, someone-19)
         |advmod(passing-17, else-20)
         |root(ROOT-0, .-21)""".stripMargin
    assert(DependencyGraph.multilineStringFormat.write(uncollapsed.collapse) === expectedCollapsedString)
    assert(graphIsValid(uncollapsed.collapse))
  }

  it should "allow collapsing output that's not a tree" in {
    val uncollapsedString =
      """|nsubj(enjoy-4, cats-1)
         |cc(cats-1, and-2)
         |conj(cats-1, dogs-3)
         |root(ROOT-0, enjoy-4)
         |det(arrow-6, an-5)
         |dobj(enjoy-4, arrow-6)
         |punct(enjoy-4, .-7)""".stripMargin
    val uncollapsed = DependencyGraph.multilineStringFormat.read(uncollapsedString)

    val expectedCollapsedString =
      """|nsubj(enjoy-4, cats-1)
         |conj_and(cats-1, dogs-3)
         |nsubj(enjoy-4, dogs-3)
         |root(ROOT-0, enjoy-4)
         |det(arrow-6, an-5)
         |dobj(enjoy-4, arrow-6)
         |punct(enjoy-4, .-7)""".stripMargin

    assert(DependencyGraph.multilineStringFormat.write(uncollapsed.collapse) === expectedCollapsedString)
    assert(graphIsValid(uncollapsed.collapse))
  }
}
