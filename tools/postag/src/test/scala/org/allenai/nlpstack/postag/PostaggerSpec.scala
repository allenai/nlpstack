package org.allenai.nlpstack.postag

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.Postagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

abstract class PostaggerSpec extends UnitSpec {
  def taggerToTest: Postagger

  "postagger implementation" should "correctly postag two example sentences" in {
    val texts = Seq(
      """|The battle station is heavily shielded and carries a firepower greater
         |than half the star fleet. Its defenses are designed around a direct,
         |large-scale assault. A small one-man fighter should be able to
         |penetrate the outer defense.""".stripMargin,
      """|Pardon me for asking, sir, but what good are snub fighters going to be
         |against that?""".stripMargin)

    val taggedTexts = Seq(
      """|The 0 DT
         |battle 4 NN
         |station 11 NN
         |is 19 VBZ
         |heavily 22 RB
         |shielded 30 VBN
         |and 39 CC
         |carries 43 VBZ
         |a 51 DT
         |firepower 53 NN
         |greater 63 JJR
         |than 71 IN
         |half 76 PDT
         |the 81 DT
         |star 85 NN
         |fleet 90 NN
         |. 95 .
         |Its 97 PRP$
         |defenses 101 NNS
         |are 110 VBP
         |designed 114 VBN
         |around 123 IN
         |a 130 DT
         |direct 132 JJ
         |, 138 ,
         |large 140 JJ
         |- 145 HYPH
         |scale 146 NN
         |assault 152 NN
         |. 159 .
         |A 161 DT
         |small 163 JJ
         |one 169 CD
         |- 172 HYPH
         |man 173 NN
         |fighter 177 NN
         |should 185 MD
         |be 192 VB
         |able 195 JJ
         |to 200 TO
         |penetrate 203 VB
         |the 213 DT
         |outer 217 JJ
         |defense 223 NN
         |. 230 .""".stripMargin,
      """|Pardon 0 VB
         |me 7 PRP
         |for 10 IN
         |asking 14 VBG
         |, 20 ,
         |sir 22 NN
         |, 25 ,
         |but 27 CC
         |what 31 WP
         |good 36 JJ
         |are 41 VBP
         |snub 45 JJ
         |fighters 50 NNS
         |going 59 VBG
         |to 65 TO
         |be 68 VB
         |against 71 IN
         |that 79 DT
         |? 83 .""".stripMargin)

    for ((text, expected) <- texts zip taggedTexts) {
      val tagged = taggerToTest.postag(defaultTokenizer)(text)
      val taggedString = tagged.mkString("\n")
      assert(taggedString === expected)
    }
  }
}
