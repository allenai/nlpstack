package org.allenai.nlpstack.coref

import org.allenai.nlpstack.core.{ Format, PostaggedToken }
import org.allenai.nlpstack.core.coref._
import org.allenai.nlpstack.core.parse.graph.{ DependencyNode, DependencyGraph }
import org.allenai.nlpstack.parse.FactorieParser

import cc.factorie.app.nlp.coref.ParseStructuredCoref
import cc.factorie.app.nlp.lemma.WordNetLemmatizer
import cc.factorie.app.nlp.ner.ConllChainNer
import cc.factorie.app.nlp.{ Token => FactorieToken, Document => FactorieDocument }

class FactorieCorefResolver extends CorefResolver[PostaggedToken] {
  def resolveCoreferences(postaggedParse: (Seq[PostaggedToken], DependencyGraph)) = {
    val factorieDoc = FactorieParser.factorieFormat.write(postaggedParse)

    // run the coreference analysis
    WordNetLemmatizer.process(factorieDoc)
    ConllChainNer.process(factorieDoc)
    ParseStructuredCoref.process(factorieDoc)

    // convert into our format
    val (tokens, tree) = postaggedParse
    def token2node(token: FactorieToken): DependencyNode =
      tree.nodes.find(_.id == tokens.indexOf(token.attr[PostaggedToken])).get

    val factorieEntities =
      factorieDoc.coref.entities.filterNot(e => e.isEmpty || e.isSingleton)
    factorieEntities.map(entity => {
      new Referent(
        entity.mentions.map(m => token2node(m.phrase.headToken)).toSeq,
        if (entity.canonicalMention == null)
          None
        else
          Some(token2node(entity.canonicalMention.phrase.headToken)))
    }).toSeq
  }
}
