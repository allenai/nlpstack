package org.allenai.nlpstack.coref

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.parse.graph.DependencyGraph
import org.allenai.nlpstack.core.PostaggedToken
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.nlpstack.parse.defaultDependencyParser

class FactorieCorefResolverSpec extends UnitSpec {
  val resolver = new FactorieCorefResolver[PostaggedToken]

  "FactorieCorefResolver" should "resolve coreferences" in {

    val text = "Our fake plants died because we did not pretend to water them."
    val tokens = defaultTokenizer.tokenize(text)
    val postaggedTokens = defaultPostagger.postagTokenized(tokens)
    val parseTree = defaultDependencyParser.dependencyGraphPostagged(postaggedTokens)

    val referents = resolver.resolveCoreferences((postaggedTokens, parseTree))

    assert(referents.size == 2, referents)
    assert(referents forall (_.references.size == 2), referents)
    val referredStrings = referents.map(_.references.map(_.string).sorted.mkString(" ")).toSet
    assert(referredStrings === Set("Our we", "plants them"))
  }
}
