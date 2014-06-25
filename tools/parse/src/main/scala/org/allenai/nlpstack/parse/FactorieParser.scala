package org.allenai.nlpstack.parse

import org.allenai.nlpstack.graph.Graph
import org.allenai.nlpstack.parse.graph.{DependencyNode, DependencyGraph}
import org.allenai.nlpstack.postag.PostaggedToken
import org.allenai.nlpstack.tokenize.Token

import cc.factorie.app.nlp.{Sentence, Document}
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.lemma.WordNetLemmatizer

class FactorieParser extends DependencyParser {
  private val parser = OntonotesTransitionBasedParser

  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]) = {
    // translate the tokens into a Factorie document
    val factorieDoc = new Document(Token.rebuildString(tokens))
    val factorieTokens = for(t <- tokens) yield {
      val factorieT = new cc.factorie.app.nlp.Token(
        factorieDoc,
        t.offset,
        t.offset + t.string.length)
      factorieT.attr += new PennPosTag(factorieT, t.postag)
      factorieT
    }
    WordNetLemmatizer.process(factorieDoc)
    val sentence = new Sentence(factorieDoc.asSection, 0, factorieDoc.tokenCount)

    // parse
    parser.process(sentence)

    // translate into our representation
    val nodes = for(t <- factorieTokens)
      yield DependencyNode(t.positionInSentence, t.string)
    def pis2node(positionInSentence: Int) =
      nodes.find(_.id == positionInSentence).get
    val edges = for(t <- factorieTokens) yield {
      val childNode = pis2node(t.positionInSentence)
      val parentNode = pis2node(t.parseParentIndex)
      new Graph.Edge[DependencyNode](
        parentNode,
        childNode,
        t.parseLabel.toString())
    }

    DependencyGraph(nodes.toSet, edges.toSet)
  }
}
