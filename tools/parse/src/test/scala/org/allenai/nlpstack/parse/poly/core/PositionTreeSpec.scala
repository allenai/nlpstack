package org.allenai.nlpstack.parse.poly.core

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParse

class PositionTreeSpec extends UnitSpec {
  // scalastyle:off

  /** This represents the following polytree parse:
    * format: OFF
    *
    * NEXUS_0
    *     |
    *     |       the_1--
    *     |              \
    *     |               -->cat_2
    *     \              /
    *      -----> sat_3--
    *        /
    * by_4 --
    *        \
    *         --> me_5
    *
    * format: ON
    */
  val parse1 = PolytreeParse(
    sentence = Sentence(Vector(NexusToken, Token('the), Token('cat), Token('sat),
      Token('by), Token('me))),
    breadcrumb = Vector(-1, 2, 3, 0, 3, 4),
    children = Vector(Set(3), Set(2), Set(), Set(2), Set(3, 5), Set()),
    arclabels = Vector(Set((3, 'root)), Set((2, 'det)), Set((1, 'det), (3, 'nsubj)),
      Set((0, 'root), (2, 'nsubj), (4, 'prep)), Set((3, 'prep), (5, 'pobj)), Set((4, 'pobj)))
  )

  val cparse1 = parse1.asConstituencyParse

  val ptree1 = PositionTree(Seq(
    (Position(Seq()), PositionTreeNode(Map('name -> "a"))),
    (Position(Seq(0)), PositionTreeNode(Map('name -> "b"))),
    (Position(Seq(1)), PositionTreeNode(Map('name -> "c"))),
    (Position(Seq(0, 0)), PositionTreeNode(Map('name -> "d"))),
    (Position(Seq(1, 0)), PositionTreeNode(Map('name -> "e"))),
    (Position(Seq(0, 1)), PositionTreeNode(Map('name -> "f")))
  ))

  "Calling Position.getChild" should "return the correct child" in {
    Position(Seq()).getChild(7) shouldBe Position(Seq(7))
    Position(Seq(3, 2, 5)).getChild(7) shouldBe Position(Seq(7, 3, 2, 5))
  }

  "Calling Position.parent" should "return the correct parent" in {
    Position(Seq(3, 2, 5)).parent shouldBe Some(Position(Seq(2, 5)))
  }

  it should "return None if called on the root position" in {
    Position.root.parent shouldBe None
  }

  "Calling Position.isDescendantOf" should "return true" in {
    Position(Seq(3, 2, 5)).isDescendentOf(Position(Seq(2, 5))) shouldBe true
    Position(Seq(3, 2, 5)).isDescendentOf(Position(Seq(5))) shouldBe true
    Position(Seq(3, 2, 5)).isDescendentOf(Position.root) shouldBe true
  }

  it should "return true when called on itself" in {
    Position(Seq(3, 2, 5)).isDescendentOf(Position(Seq(3, 2, 5))) shouldBe true
  }

  it should "return false" in {
    Position(Seq(2, 5)).isDescendentOf(Position(Seq(3, 2, 5))) shouldBe false
    Position(Seq(2, 5)).isDescendentOf(Position(Seq(3, 5))) shouldBe false
    Position(Seq(2, 5)).isDescendentOf(Position(Seq(2, 4))) shouldBe false
  }

  "Calling Position.depthFirstPreorder" should "return the correct sorted order" in {
    Position.depthFirstPreorder(Seq(
      Position(Seq(0, 0)), Position(Seq(0)), Position(Seq(1, 0)),
      Position(Seq(1)), Position(Seq(0, 1)), Position.root
    )) shouldBe {
      Seq(
        Position.root, Position(Seq(0)), Position(Seq(0, 0)), Position(Seq(1, 0)),
        Position(Seq(1)), Position(Seq(0, 1))
      )
    }
  }

  "Calling PositionTree.getChildren" should "return the correct children" in {
    cparse1.getChildren(Position.root) shouldBe Seq(
      Position(Seq(0)), Position(Seq(1)), Position(Seq(2))
    )
    cparse1.getChildren(Position(Seq(2))) shouldBe Seq(
      Position(Seq(0, 2)), Position(Seq(1, 2))
    )
    cparse1.getChildren(Position(Seq(0, 2))) shouldBe Seq.empty
  }

  "Calling PositionTree.getLabel" should "return the correct label" in {
    cparse1.getLabel(Position(Seq(0)), ConstituencyParse.constituencyLabelName) shouldBe
      Some("nsubj")
    cparse1.getLabel(Position(Seq(1, 0)), ConstituencyParse.wordLabelName) shouldBe
      Some("cat")
  }

  it should "return None for an invalid position" in {
    cparse1.getLabel(Position(Seq(3)), ConstituencyParse.constituencyLabelName) shouldBe None
  }

  it should "return None for an invalid label" in {
    cparse1.getLabel(Position(Seq(0)), ConstituencyParse.wordLabelName) shouldBe None
  }

  "Calling PositionTree.leaves" should "return the correct leaves" in {
    cparse1.leaves shouldBe Seq(
      Position(Seq(0, 0, 0)), Position(Seq(1, 0)), Position(Seq(1)), Position(Seq(0, 2)),
      Position(Seq(0, 1, 2))
    )
  }

  "Calling PositionTree.swapPositions" should "return a tree with swapped subtrees" in {
    ptree1.swapPositions(Position(Seq(0)), Position(Seq(1))).positions.toSet shouldBe {
      PositionTree(Seq(
        (Position(Seq()), PositionTreeNode(Map('name -> "a"))),
        (Position(Seq(1)), PositionTreeNode(Map('name -> "b"))),
        (Position(Seq(0)), PositionTreeNode(Map('name -> "c"))),
        (Position(Seq(0, 1)), PositionTreeNode(Map('name -> "d"))),
        (Position(Seq(1, 1)), PositionTreeNode(Map('name -> "e"))),
        (Position(Seq(0, 0)), PositionTreeNode(Map('name -> "f")))
      )).positions.toSet
    }
  }

  "Calling PositionTree.substitute" should "return a tree with a replaced subtree" in {
    ptree1.substitute(Position(Seq(1)), ptree1).positions.toSet shouldBe {
      PositionTree(Seq(
        (Position(Seq()), PositionTreeNode(Map('name -> "a"))),
        (Position(Seq(0)), PositionTreeNode(Map('name -> "b"))),
        (Position(Seq(0, 0)), PositionTreeNode(Map('name -> "d"))),
        (Position(Seq(1, 0)), PositionTreeNode(Map('name -> "e"))),
        (Position(Seq(1)), PositionTreeNode(Map('name -> "a"))),
        (Position(Seq(0, 1)), PositionTreeNode(Map('name -> "b"))),
        (Position(Seq(1, 1)), PositionTreeNode(Map('name -> "c"))),
        (Position(Seq(0, 0, 1)), PositionTreeNode(Map('name -> "d"))),
        (Position(Seq(1, 0, 1)), PositionTreeNode(Map('name -> "e"))),
        (Position(Seq(0, 1, 1)), PositionTreeNode(Map('name -> "f")))
      )).positions.toSet
    }
  }
}
