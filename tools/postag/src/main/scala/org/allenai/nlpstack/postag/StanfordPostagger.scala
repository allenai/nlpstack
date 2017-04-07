package org.allenai.nlpstack.postag

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.tagger.maxent.MaxentTagger

import org.allenai.nlpstack.core._
import org.allenai.datastore.Datastore

import java.net.URL
import scala.collection.JavaConverters._

class StanfordPostagger(
    val tagger: MaxentTagger
) extends Postagger {

  def this() = this(StanfordPostagger.loadDefaultModel())

  override def postagTokenized(tokens: Seq[Token]): Seq[PostaggedToken] = {
    val labels = tokens.map { token =>
      val corelabel = new CoreLabel()
      corelabel.setWord(token.string)
      corelabel
    }
    val postags = tagger.tagSentence(labels.asJava).asScala.map(_.tag())

    (tokens zip postags).map {
      case (token, postag) =>
        PostaggedToken(token, postag)
    }
  }
}

object StanfordPostagger {
  def loadDefaultModel(): MaxentTagger = {
    val filePath = Datastore.directoryPath(
      "edu.stanford.nlp.models.pos-tagger",
      "english-left3words-3.4.1",
      1
    )
    new MaxentTagger(filePath.toString + "/english-left3words/english-left3words-distsim.tagger")
  }
}
