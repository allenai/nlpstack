package org.allenai.nlpstack.coref

import org.allenai.nlpstack.core.{ PostaggedToken, Token }
import org.allenai.nlpstack.core.coref._
import org.allenai.nlpstack.core.parse.graph.DependencyGraph

import cc.factorie.app.nlp.coref.ParseStructuredCoref
import cc.factorie.app.nlp.lemma.WordNetLemmatizer
import cc.factorie.app.nlp.ner.ConllChainNer
import cc.factorie.app.nlp.parse.{ ParseTreeLabelDomain, ParseTree }
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.{ Sentence, Token => FactorieToken, Document }

class FactorieCorefResolver[T <: PostaggedToken] extends CorefResolver[T] {
  def resolveCoreferences(postaggedParse: (Seq[T], DependencyGraph)) = {
    /** We have to box the token to be able to stick it into Factorie's attr
      * system. We use it to associate a factorie token with our own token.
      */
    case class BoxedToken(val t: T);

    // translate the input into a factorie document
    val (tokens, tree) = postaggedParse
    val factorieDoc = new Document(Token.rebuildString(tokens))
    val factorieTokens = for (token <- tokens) yield {
      val factorieT = new FactorieToken(
        factorieDoc,
        token.offset,
        token.offset + token.string.length)
      factorieT.attr += new PennPosTag(factorieT, token.postag)
      factorieT.attr += BoxedToken(token)
      factorieT
    }
    WordNetLemmatizer.process(factorieDoc)
    val sentence = new Sentence(factorieDoc.asSection, 0, factorieDoc.tokenCount)
    val factorieParse = sentence.attr.getOrElseUpdate(new ParseTree(sentence))
    for (vertex <- tree.vertices) {
      val incomingEdges = tree.incoming(vertex)
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

    // run the coreference analysis
    ConllChainNer.process(factorieDoc)
    ParseStructuredCoref.process(factorieDoc)

    // convert into our format
    val factorieEntities =
      factorieDoc.coref.entities.filterNot(e => e.isEmpty || e.isSingleton)
    factorieEntities.map(entity => {
      new Referent[T](
        entity.mentions.map(_.phrase.headToken.attr[BoxedToken].t).toSeq,
        if (entity.canonicalMention == null)
          None
        else
          Some(entity.canonicalMention.phrase.headToken.attr[BoxedToken].t))
    }).toSeq
  }
}

