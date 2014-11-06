package org.allenai.nlpstack.parse

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.core.graph.Graph
import org.allenai.nlpstack.core.parse.graph.{ DependencyGraph, DependencyNode }
import org.allenai.nlpstack.postag.FactoriePostagger

import cc.factorie.app.nlp.lemma.WordNetLemmatizer
import cc.factorie.app.nlp.parse.{ ParseTreeLabelDomain, ParseTree, OntonotesTransitionBasedParser }
import cc.factorie.app.nlp.{ Document => FactorieDocument, Sentence => FactorieSentence }

class FactorieParser extends DependencyParser {
  override def dependencyGraphPostagged(tokens: Seq[PostaggedToken]) = {
    val factorieDoc = FactoriePostagger.factorieFormat.write(tokens)

    WordNetLemmatizer.process(factorieDoc)
    val sentence = new FactorieSentence(factorieDoc.asSection, 0, factorieDoc.tokenCount)
    OntonotesTransitionBasedParser.process(sentence)

    FactorieParser.factorieFormat.read(factorieDoc)._2
  }
}

object FactorieParser {
  object factorieFormat extends Format[(Seq[PostaggedToken], DependencyGraph), FactorieDocument] {
    override def read(from: FactorieDocument): (Seq[PostaggedToken], DependencyGraph) = {
      val nodes = from.tokens.map(t =>
        DependencyNode(t.positionInSentence, t.string))

      def pis2node(positionInSentence: Int) =
        nodes.find(_.id == positionInSentence).getOrElse(
          sys.error("No token with PIS " + positionInSentence)
        )
      // Since the number of tokens in the sentence will be small most of the
      // time, we don't need to optimize with a map that would do this lookup
      // quicker.
      val edges = for (t <- from.tokens if t.parseParentIndex >= 0) yield {
        val parentNode = pis2node(t.parseParentIndex)
        val childNode = pis2node(t.positionInSentence)
        new Graph.Edge[DependencyNode](
          parentNode,
          childNode,
          t.parseLabel.categoryValue
        )
      }

      require(from.sentenceCount == 1)
      val firstRoot = pis2node(from.sentences.head.parseRootChild.positionInSentence)
      val dgraph = DependencyGraph(Some(firstRoot), nodes.toSet, edges.toSet)

      val postaggedTokens = FactoriePostagger.factorieFormat.read(from)

      (postaggedTokens, dgraph)
    }

    override def write(from: (Seq[PostaggedToken], DependencyGraph)): FactorieDocument = {
      val factorieDoc = FactoriePostagger.factorieFormat.write(from._1)
      val sentence = new FactorieSentence(
        factorieDoc.asSection,
        0,
        factorieDoc.tokenCount
      )
      val factorieParse = sentence.attr.getOrElseUpdate(new ParseTree(sentence))
      for (vertex <- from._2.vertices) {
        val incomingEdges = from._2.incoming(vertex)
        require(incomingEdges.size <= 1, "Parse tree is not a tree")
        if (incomingEdges.isEmpty) {
          factorieParse.setParent(vertex.id, -1)
          factorieParse.label(vertex.id).set(ParseTreeLabelDomain.index(""))(null)
        } else {
          val parentEdge = incomingEdges.head
          val parent = parentEdge.source
          factorieParse.setParent(vertex.id, parent.id)
          factorieParse.label(vertex.id).set(ParseTreeLabelDomain.index(parentEdge.label))(null)
        }
      }

      factorieDoc
    }
  }
}
