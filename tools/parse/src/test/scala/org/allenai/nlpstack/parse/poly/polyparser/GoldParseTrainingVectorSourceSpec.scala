package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.parse.poly.core.{ Sentence, NexusToken, Token }
import org.allenai.nlpstack.parse.poly.fsm.{
  FSMTrainingVectorSource,
  FSMTrainingVector,
  FeatureUnion
}
import org.allenai.nlpstack.parse.poly.ml.{ FeatureVector, FeatureName }

class GoldParseTrainingVectorSourceSpec extends UnitSpec {
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
    arclabels =
      Vector(
        Set((3, SingleSymbolArcLabel('root))),
        Set((2, SingleSymbolArcLabel('det))),
        Set((1, SingleSymbolArcLabel('det)), (3, SingleSymbolArcLabel('nsubj))),
        Set((0, SingleSymbolArcLabel('root)), (2, SingleSymbolArcLabel('nsubj)),
          (4, SingleSymbolArcLabel('prep))),
        Set((3, SingleSymbolArcLabel('prep)), (5, SingleSymbolArcLabel('pobj))),
        Set((4, SingleSymbolArcLabel('pobj)))
      )
  )
  val parseSource = InMemoryPolytreeParseSource(List(parse1))

  /** TODO: These unit tests are currently disabled because the transition systems temporarily
    * do not permit features to be specified during construction. They will be re-enabled when
    * these functionality returns. */
  /*
   "Calling TrainingVectorGenerator's generate" should "create parse1's feature vectors" in {
    val feat = new FeatureUnion(List(
      new TokenTransformFeature(StackRef(0), Set(WordTransform)),
      new TokenTransformFeature(BufferRef(0), Set(WordTransform))
    ))
    val generator = new GoldParseTrainingVectorSource(parseSource, ArcEagerTransitionSystem(feat))
    val vectors: IndexedSeq[FSMTrainingVector] = generator.getVectorIterator.toIndexedSeq
    vectors.size shouldBe 10
    vectors(0).task shouldBe ApplicabilitySignature(true, false, false, true)
    vectors(0).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'nexus)) -> 1,
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'the)) -> 1
      ))
    vectors(0).transition shouldBe ArcEagerShift
    vectors(1).task shouldBe ApplicabilitySignature(true, false, true, true)
    vectors(1).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'the)) -> 1,
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'cat)) -> 1
      ))
    vectors(1).transition shouldBe ArcEagerInvertedLeftArc('det)
    vectors(2).task shouldBe ApplicabilitySignature(true, false, false, true)
    vectors(2).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'nexus)) -> 1,
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'cat)) -> 1
      ))
    vectors(2).transition shouldBe ArcEagerShift
    vectors(3).task shouldBe ApplicabilitySignature(true, false, true, true)
    vectors(3).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'cat)) -> 1,
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'sat)) -> 1
      ))
    vectors(3).transition shouldBe ArcEagerLeftArc('nsubj)
    vectors(4).task shouldBe ApplicabilitySignature(true, false, false, true)
    vectors(4).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'nexus)) -> 1,
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'sat)) -> 1
      ))
    vectors(4).transition shouldBe ArcEagerRightArc('root)
    vectors(5).task shouldBe ApplicabilitySignature(true, false, false, true)
    vectors(5).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'sat)) -> 1,
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'by)) -> 1
      ))
    vectors(5).transition shouldBe ArcEagerInvertedRightArc('prep)
    vectors(6).task shouldBe ApplicabilitySignature(false, true, false, true)
    vectors(6).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'by)) -> 1,
        FeatureName(List(WordTransform.name, BufferRef(0).name, 'me)) -> 1
      ))
    vectors(6).transition shouldBe ArcEagerRightArc('pobj)
    vectors(7).task shouldBe ApplicabilitySignature(false, true, false, false)
    vectors(7).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'me)) -> 1
      ))
    vectors(7).transition shouldBe ArcEagerReduce
    vectors(8).task shouldBe ApplicabilitySignature(false, true, false, false)
    vectors(8).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'by)) -> 1
      ))
    vectors(8).transition shouldBe ArcEagerReduce
    vectors(9).task shouldBe ApplicabilitySignature(false, true, false, false)
    vectors(9).featureVector shouldBe
      FeatureVector(Seq(
        FeatureName(List(WordTransform.name, StackRef(0).name, 'sat)) -> 1
      ))
    vectors(9).transition shouldBe ArcEagerReduce
  }

  "Calling collectTransitions" should "find all transitions in parse1's feature vectors" in {
    val feat = new FeatureUnion(List(
      new TokenTransformFeature(StackRef(0), Set(WordTransform)),
      new TokenTransformFeature(BufferRef(0), Set(WordTransform))
    ))
    val vectorSource = new GoldParseTrainingVectorSource(
      parseSource,
      ArcEagerTransitionSystem(feat)
    )
    FSMTrainingVectorSource.collectTransitions(vectorSource) shouldBe Set(ArcEagerShift, ArcEagerReduce,
      ArcEagerInvertedLeftArc('det), ArcEagerLeftArc('nsubj), ArcEagerRightArc('root), ArcEagerInvertedRightArc('prep),
      ArcEagerRightArc('pobj))
  }

  "Calling collectFeatureNames" should "find all feature names in parse1's feature vectors" in {
    val feat = new FeatureUnion(List(
      new TokenTransformFeature(StackRef(0), Set(WordTransform)),
      new TokenTransformFeature(BufferRef(0), Set(WordTransform))
    ))
    val vectorSource = new GoldParseTrainingVectorSource(
      parseSource,
      ArcEagerTransitionSystem(feat)
    )
    FSMTrainingVectorSource.collectFeatureNames(vectorSource) shouldBe (Set(
      List(WordTransform.name, StackRef(0).name, 'nexus),
      List(WordTransform.name, StackRef(0).name, 'the),
      List(WordTransform.name, StackRef(0).name, 'cat),
      List(WordTransform.name, StackRef(0).name, 'sat),
      List(WordTransform.name, StackRef(0).name, 'by),
      List(WordTransform.name, StackRef(0).name, 'me),
      List(WordTransform.name, BufferRef(0).name, 'the),
      List(WordTransform.name, BufferRef(0).name, 'cat),
      List(WordTransform.name, BufferRef(0).name, 'sat),
      List(WordTransform.name, BufferRef(0).name, 'by),
      List(WordTransform.name, BufferRef(0).name, 'me)
    ) map
      { symbols: List[Symbol] => FeatureName(symbols) })
  }
  */
}
