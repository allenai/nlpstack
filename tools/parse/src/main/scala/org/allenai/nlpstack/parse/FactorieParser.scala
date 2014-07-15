package org.allenai.nlpstack.parse

import org.allenai.nlpstack.core.graph.Graph
import org.allenai.nlpstack.core.parse.{ DependencyParserMain, DependencyParser }
import org.allenai.nlpstack.core.parse.graph.{ DependencyNode, DependencyGraph }
import org.allenai.nlpstack.core.postag.PostaggedToken
import org.allenai.nlpstack.core.tokenize.Token
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.tokenize._

import cc.factorie.app.nlp.{ Sentence, Document }
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.{ Token => FactorieToken }
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.lemma.WordNetLemmatizer

class FactorieParser extends DependencyParser {
  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]) = {
    // translate the tokens into a Factorie document
    val factorieDoc = new Document(Token.rebuildString(tokens))
    val factorieTokens = for (token <- tokens) yield {
      val factorieT = new FactorieToken(
        factorieDoc,
        token.offset,
        token.offset + token.string.length)
      factorieT.attr += new PennPosTag(factorieT, token.postag)
      factorieT
    }
    WordNetLemmatizer.process(factorieDoc)
    val sentence = new Sentence(factorieDoc.asSection, 0, factorieDoc.tokenCount)

    // parse
    OntonotesTransitionBasedParser.process(sentence)

    // translate into our representation
    val nodes = for (t <- factorieTokens) yield {
      DependencyNode(t.positionInSentence, t.string)
    }
    def pis2node(positionInSentence: Int) =
      nodes.find(_.id == positionInSentence).getOrElse(
        sys.error("No token with PIS " + positionInSentence))
    // Since the number of tokens in the sentence will be small most of the
    // time, we don't need to optimize with a map that would do this lookup
    // quicker.
    val edges = for (t <- factorieTokens if t.parseParentIndex >= 0) yield {
      val parentNode = pis2node(t.parseParentIndex)
      val childNode = pis2node(t.positionInSentence)
      new Graph.Edge[DependencyNode](
        parentNode,
        childNode,
        t.parseLabel.categoryValue)
    }

    val firstRoot = pis2node(sentence.parseRootChild.positionInSentence)

    DependencyGraph(Some(firstRoot), nodes.toSet, edges.toSet)
  }
}

object FactorieParserMain extends DependencyParserMain {
  override lazy val tokenizer = defaultTokenizer
  override lazy val postagger = defaultPostagger
  override lazy val dependencyParser = new FactorieParser
}
