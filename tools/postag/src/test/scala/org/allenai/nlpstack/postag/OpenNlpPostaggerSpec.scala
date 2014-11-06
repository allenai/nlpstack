package org.allenai.nlpstack
package postag

class OpenNlpPostaggerSpec extends PostaggerSpec {
  // The OpenNLP postagger disagrees about the tags for "Pardon", where it is wrong, and "snub",
  // where the difference is acceptable.
  protected override def taggedTexts = Seq(
    super.taggedTexts(0),
    """|Pardon 0 NNP
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
       |snub 45 NN
       |fighters 50 NNS
       |going 59 VBG
       |to 65 TO
       |be 68 VB
       |against 71 IN
       |that 79 DT
       |? 83 .""".stripMargin
  )

  val taggerToTest = new OpenNlpPostagger
}

