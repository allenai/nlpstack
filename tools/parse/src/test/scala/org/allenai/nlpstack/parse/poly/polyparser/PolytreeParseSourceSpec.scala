package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Token, NexusToken, Sentence }

class PolytreeParseSourceSpec extends UnitSpec {


  /** This represents the following polytree parse:
    * format: OFF
    *
    * NEXUS_0
    * |
    * |                -->he_1
    * \               /
    * -----> ate_2 --
    * /          \
    * |            -->pasta_3
    * |
    * with_4 --
    * \
    * -->meatballs_5
    *
    * format: ON
    */
  val parse1 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('he, Map('cpos -> Set('NN))),
      Token('ate, Map('cpos -> Set('VB))),
      Token('pasta, Map('cpos -> Set('NN))),
      Token('with, Map('cpos -> Set('IN))),
      Token('meatballs, Map('cpos -> Set(Symbol("."))))
    )),
    breadcrumb = Vector(-1, 2, 0, 2, 2, 4),
    children = Vector(Set(2), Set(), Set(1, 3), Set(), Set(2, 5), Set()),
    arclabels =
      Vector(
        Set((2, SingleSymbolArcLabel('CONJ))),
        Set((2, SingleSymbolArcLabel('NSUBJ))),
        Set((1, SingleSymbolArcLabel('NSUBJ)), (3, SingleSymbolArcLabel('DOBJ)),
          (4, SingleSymbolArcLabel('PREP)), (0, SingleSymbolArcLabel('CONJ))),
        Set((2, SingleSymbolArcLabel('DOBJ))),
        Set((2, SingleSymbolArcLabel('PREP)), (5, SingleSymbolArcLabel('POBJ))),
        Set((4, SingleSymbolArcLabel('POBJ)))
      )
  )

  /** This represents the following polytree parse:
    *
    * NEXUS_0
    *     |
    *     |       the_1--
    *     |              \
    *     |               -->cat_2
    *     \              /
    *      -----> gave_3-
    *                   \ \
    *                   \  -->me_4
    *                   \
    *                    --> it_5
    *
    */
  val parse2 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('the, Map('cpos -> Set('DT))),
      Token('cat, Map('cpos -> Set('NN))),
      Token('gave, Map('cpos -> Set('VB))),
      Token('me, Map('cpos -> Set('NN))),
      Token('it, Map('cpos -> Set('NN)))
    )),
    breadcrumb = Vector(-1, 2, 3, 0, 3, 3),
    children = Vector(Set(3), Set(2), Set(), Set(2, 4, 5), Set(), Set()),
    arclabels =
      Vector(
        Set((3, SingleSymbolArcLabel('root))),
        Set((2, SingleSymbolArcLabel('det))),
        Set((1, SingleSymbolArcLabel('det)), (3, SingleSymbolArcLabel('nsubj))),
        Set((0, SingleSymbolArcLabel('root)), (2, SingleSymbolArcLabel('nsubj)),
          (4, SingleSymbolArcLabel('dobj)), (5, SingleSymbolArcLabel('iobj))),
        Set((3, SingleSymbolArcLabel('dobj))),
        Set((4, SingleSymbolArcLabel('iobj)))
      )
  )

  "PolytreeParseSource.countTokens" should "return the correct answer" in {
    val parseSource = InMemoryPolytreeParseSource(Seq(parse1, parse2))
    PolytreeParseSource.countTokens(parseSource, excludePunctuation=false) shouldBe 10
    PolytreeParseSource.countTokens(parseSource, excludePunctuation=true) shouldBe 9
  }
}


