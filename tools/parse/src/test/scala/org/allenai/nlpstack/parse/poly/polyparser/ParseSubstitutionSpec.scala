package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ VariableToken, Token, NexusToken, Sentence }

class ParseSubstitutionSpec extends UnitSpec {

  val vtree1 = ParseSubstitution.constructVariableParse(
    "yesterday|NN $0 sat|VB by|IN me|PRP",
    "3|TMOD 3|NSUBJ 0|ROOT 3|PREP 4|POBJ"
  )

  val vtree2 = ParseSubstitution.constructVariableParse(
    "the|DT cat|NN in|IN the|DT hat|NN",
    "1|DET 0|ROOT 1|PREP 4|DET 2|POBJ"
  )

  val parse1 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('yesterday, Map('cpos -> Set('NN))),
      new VariableToken(0),
      Token('sat, Map('cpos -> Set('VB))),
      Token('by, Map('cpos -> Set('IN))),
      Token('me, Map('cpos -> Set('PRP)))
    )),
    breadcrumb = Vector(-1, 3, 3, 0, 3, 4),
    children = Vector(Set(3), Set(3), Set(), Set(2), Set(3, 5), Set()),
    arclabels =
      Vector(
        Set((3, SingleSymbolArcLabel('ROOT))),
        Set((3, SingleSymbolArcLabel('TMOD))),
        Set((3, SingleSymbolArcLabel('NSUBJ))),
        Set((0, SingleSymbolArcLabel('ROOT)), (1, SingleSymbolArcLabel('TMOD)), (2, SingleSymbolArcLabel('NSUBJ)),
          (4, SingleSymbolArcLabel('PREP))),
        Set((3, SingleSymbolArcLabel('PREP)), (5, SingleSymbolArcLabel('POBJ))),
        Set((4, SingleSymbolArcLabel('POBJ)))
      )
  )

  val parse2 = PolytreeParse(
    sentence = Sentence(Vector(
      NexusToken,
      Token('cat, Map('cpos -> Set('NN))),
      Token('in, Map('cpos -> Set('IN))),
      Token('the, Map('cpos -> Set('DT))),
      Token('hat, Map('cpos -> Set('NN)))
    )),
    breadcrumb = Vector(-1, 0, 1, 4, 2),
    children = Vector(Set(1), Set(), Set(1, 4), Set(4), Set()),
    arclabels =
      Vector(
        Set((1, SingleSymbolArcLabel('ROOT))),
        Set((0, SingleSymbolArcLabel('ROOT)), (2, SingleSymbolArcLabel('ADP))),
        Set((1, SingleSymbolArcLabel('ADP)), (4, SingleSymbolArcLabel('POBJ))),
        Set((4, SingleSymbolArcLabel('DET))),
        Set((3, SingleSymbolArcLabel('DET)), (2, SingleSymbolArcLabel('POBJ)))
      )
  )

  "Calling substitute" should "do the right thing" in {
    println(ParseSubstitution.substitute(parse1, Map(0 -> parse2)).asConllX)
    println(s"parse: ${vtree1.asConllX}")
    println(ParseSubstitution.substitute(vtree1, Map(0 -> vtree2)).asConllX)
    Range(0, 5) map { x =>
      println("\n")
      println(ParseSubstitution.generateEquationTree(0.2).asConllX)
    }

  }
}
