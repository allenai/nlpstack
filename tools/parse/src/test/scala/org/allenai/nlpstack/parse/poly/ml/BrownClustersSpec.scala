package org.allenai.nlpstack.parse.poly.ml


import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{Token, Sentence}


class BrownClustersSpec extends UnitSpec {

  val clusters1 = BrownClusters.fromStringMap(Map(
    ("apple", "00"),
    ("cherry", "00"),
    ("banana", "01"),
    ("carrot", "100"),
    ("beet", "101"),
    ("turnip", "101"),
    ("celery", "11")
  ), Map())

  val clusters2 = BrownClusters.fromStringMap(Map(
    ("apple", "10"),
    ("beet", "01")
  ), Map())

  val sentence1 = Sentence(IndexedSeq(Token('apple), Token('and), Token('cherry), Token('beet)))


  "BrownClusters.getAllClusters" should "return the correct answer" in {
    clusters1.getAllClusters('turnip).size shouldBe 4
    clusters1.getAllClusters('turnip) shouldBe clusters1.getAllClusters('beet)
    clusters1.getAllClusters('turnip) == clusters1.getAllClusters('carrot) shouldBe false
  }

  it should "return zero for an unknown word" in {
    clusters1.getAllClusters('rutabaga) shouldBe List(Symbol("0"))
  }
/*
  "Sentence.taggedWithBrownClusters" should "return the correct answer" in {
    sentence1.taggedWithBrownClusters(Seq(clusters1, clusters2)) shouldBe
      Sentence(Seq(
        Token('apple, Map('brown0 -> Set(Symbol("0"), Symbol("00")),
          'brown1 -> Set(Symbol("1"), Symbol("10")))),
        Token('and, Map('brown0 -> Set[Symbol](),
          'brown1 -> Set[Symbol]())),
        Token('cherry, Map('brown0 -> Set(Symbol("0"), Symbol("00")),
          'brown1 -> Set[Symbol]())),
        Token('beet, Map('brown0 -> Set(Symbol("1"), Symbol("10"), Symbol("101")),
          'brown1 -> Set(Symbol("0"), Symbol("01"))))))
  }
*/
}

