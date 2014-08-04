package org.allenai.nlpstack.core

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.coref.{ CorefResolver, Referent }
import org.allenai.nlpstack.core.parse.graph.DependencyGraph

class CorefResolverSpec extends UnitSpec {
  "CorefResolverSerialization" should "round trip through serialization" in {
    val dgraphString =
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
    val dgraph = DependencyGraph.multilineStringFormat.read(dgraphString)

    // minus 1 because the dgraph's serialization format increases the numbers
    // by one
    val amphibians = dgraph.nodeById(3 - 1).get
    val they = dgraph.nodeById(16 - 1).get

    for (
      format <- Seq(CorefResolver.multilineStringFormat, CorefResolver.singlelineStringFormat);
      mainReference <- Seq(Some(amphibians), None)
    ) {
      val coref = Seq(Referent(Seq(amphibians, they), mainReference))
      val corefString = format.write((dgraph, coref))
      val newCoref = format.read(corefString)
      assert((dgraph, coref) === newCoref)
    }
  }
}
