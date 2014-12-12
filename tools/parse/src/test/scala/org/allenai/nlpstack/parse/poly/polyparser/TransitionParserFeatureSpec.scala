package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{AnnotatedSentence, Sentence, NexusToken, Token}
import org.allenai.nlpstack.parse.poly.fsm.{StateFeature, FeatureUnion}
import org.allenai.nlpstack.parse.poly.ml.{FeatureVector, FeatureName}
import spray.json._

object TransitionParserFeatureTestData {
  // scalastyle:off
}

class TransitionParserFeatureSpec extends UnitSpec {
  // scalastyle:off

  /** This represents the following parser state:
    *
    *   ---------
    *   |        |
    *   |        V
    * NEXUS_0  saw_2  |||  cat_5  with_6  a_7  telescope_8
    *            |         /   \
    *            V        V    V


    */
  val state1: TransitionParserState = TransitionParserState(
    stack = Vector(2, 0),
    bufferPosition = 5,
    breadcrumb = Map(0 -> -1, 1 -> 2, 2 -> 0, 3 -> 5, 4 -> 5),
    children = Map(0 -> Set(2), 2 -> Set(1), 5 -> Set(3, 4)),
    arcLabels = Map(Set(0, 2) -> 'root, Set(2, 1) -> 'nsubj, Set(3, 5) -> 'det, Set(4, 5) -> 'amod),
    annotatedSentence = AnnotatedSentence(
      Sentence(Vector(NexusToken, Token('we), Token('saw), Token('a),
        Token('white), Token('cat), Token('with), Token('a), Token('telescope))),
      IndexedSeq()))

  /*
  "Calling TokenTransformFeature's apply" should "give back a singleton map" in {
    val feat = new TokenTransformFeature(StackRef(0), WordTransform)
    feat(state1) shouldBe
      FeatureVector(Map(FeatureName(List(WordTransform.name, StackRef(0).name, 'saw)) -> 1))
  }


  //it should "give back a map with two mappings" in {
  //  val feat = new TokenTransformFeature(BufferRef(0), ChildLabels)
  //  feat(state1) shouldBe
  //    FeatureVector(Map(FeatureName(List(ChildLabels.name, BufferRef(0).name, 'det)) -> 1,
  //      FeatureName(List(ChildLabels.name, BufferRef(0).name, 'amod)) -> 1))
  //}


  it should "give back an empty map when the feature doesn't apply" in {
    val feat = new TokenTransformFeature(StackRef(3), WordTransform)
    feat(state1) shouldBe FeatureVector(Map())
  }

  "Serializing a TokenTransformFeature" should "preserve the object state" in {
    val feature: StateFeature = TokenTransformFeature(StackRef(3), WordTransform)
    feature.toJson.convertTo[StateFeature] shouldBe
      TokenTransformFeature(StackRef(3), WordTransform)
  }

  "Calling FeatureUnion's apply" should "give back the union of all subfeatures" in {
    val feat = new FeatureUnion(List(new TokenTransformFeature(StackRef(0), WordTransform),
      new TokenTransformFeature(BufferRef(0), WordTransform)))
    feat(state1) shouldBe
      FeatureVector(Map(
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'cat)) -> 1,
        FeatureName(List(WordTransform.name, StackRef(0).name, 'saw)) -> 1))
  }

  "Serializing a FeatureUnion" should "preserve the object state" in {
    val feature: StateFeature = FeatureUnion(
      List(TokenTransformFeature(StackRef(3), WordTransform),
        TokenTransformFeature(BufferRef(2), TokenPropertyTransform('cpos))))
    feature.toJson.convertTo[StateFeature] shouldBe FeatureUnion(
        List(TokenTransformFeature(StackRef(3), WordTransform),
          TokenTransformFeature(BufferRef(2), TokenPropertyTransform('cpos))))
  }
  */
}
