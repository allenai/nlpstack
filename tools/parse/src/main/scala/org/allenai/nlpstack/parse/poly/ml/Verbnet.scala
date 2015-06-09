package org.allenai.nlpstack.parse.poly.ml

import org.allenai.datastore._

import edu.mit.jverbnet.data._
import edu.mit.jverbnet.index._
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import reming.DefaultJsonProtocol._

import scala.collection.JavaConverters._

/** A class that uses JVerbnet, a 3rd party Wrapper library for Verbnet data
  * (http://projects.csail.mit.edu/jverbnet/),  to quickly look up various verbnet
  * features for a verb.
  */
case class Verbnet(groupName: String, artifactName: String, version: Int) {

  // Construct the index and open it
  @transient val index = {
    val verbnetPath = Datastore.directoryPath(
      groupName,
      artifactName,
      version
    )
    val url = verbnetPath.toUri.toURL
    val ix = new VerbIndex(url)
    ix.open
    ix
  }

  // Construct a table mapping a word (verb) to the set of verbnet classes it can be part of.
  @transient private val verbnetClassTable = {
    val table = scala.collection.mutable.HashMap.empty[Symbol, Set[IVerbClass]]
    // Iterate through verb classes, and for each, extract the verb and
    // its synonyms. Populate synonym map.
    val vList = index.iterator.asScala
    for (vClass <- vList) {
      for (classMember <- vClass.getMembers.asScala) {
        val member = Symbol(classMember.getName.trim)
        table(member) = table.getOrElse(member, Set.empty) + vClass
      }
    }
    table.toMap
  }

  /* Returns the set of  all classes for a given verb.
  .*/
  def getVerbnetClasses(verb: String): Set[IVerbClass] = {
    val verbSym = Symbol(verb)
    verbnetClassTable.getOrElse(verbSym, Set.empty[IVerbClass])
  }

  /* Returns the names of all classes for a given verb.
  .*/
  def getVerbnetClassNames(verb: String): Set[Symbol] = {
    val verbClasses = getVerbnetClasses(verb)
    verbClasses.map(x => Symbol(x.getID))
  }

  /* Returns the set of all frames within all classes for a given verb.
   */
  def getVerbnetFrames(verb: String): Set[IFrame] = {
    // Get all classes
    val verbClasses = getVerbnetClasses(verb)
    verbClasses flatMap { verbClass => verbClass.getFrames.asScala }
  }

  /* Returns the set of primary names for all frames within all classes
   * for a given verb.
   */
  def getVerbnetFramePrimaryNames(verb: String): Set[Symbol] = {
    // Get frames
    val verbFrames = getVerbnetFrames(verb)
    // Split the frame name into its constituents, for e.g., "NP", "V" and "PP.result" from
    // the fram name "NP V PP.result". Then strip the part following a dot, if it exists, from
    // each of the constituents. For e.g., "PP.result" becomes "PP", so the entire frame name
    // will become "NP V PP". Then replace whitespaces with dashes, to get "NP-V-PP".
    val result = (for {
      verbFrame <- verbFrames
    } yield {
      val primaryName = verbFrame.getPrimaryType.getID
      val constituents = primaryName.split("""\s+""")
      val modConstituents = for {
        constituent <- constituents
      } yield {
        val pattern = """^(.+)\..+$""".r
        val patternMatch = pattern.findFirstMatchIn(constituent)
        patternMatch match {
          case Some(x) => x.group(1)
          case None => constituent
        }
      }
      modConstituents.mkString("-")
    }).map(x => Symbol(x)).toSet
    result
  }

  /* Returns the set of secondary names for all frames within all classes
   * for a given verb.
   */
  def getVerbnetFrameSecondaryNames(verb: String): Set[Symbol] = {
    // Get frames
    val verbFrames = getVerbnetFrames(verb)
    val result = verbFrames.map(_.getSecondaryType).filter(secondaryType => secondaryType != null).map(
      secondaryType => Symbol(secondaryType.getID.replaceAll("""\s+""", "-"))
    )
    result
  }
}

object Verbnet {
  implicit val jsonFormat = jsonFormat3(Verbnet.apply)
}

/** A SentenceTagger that tags sentence tokens using Verbnet frames.
  *
  * @param verbnet the associated Verbnet resource
  * @param useSecondaryFrames set to true if you want secondary (rather than primary) frames
  */
case class VerbnetTagger(
    verbnet: Verbnet,
    useSecondaryFrames: Boolean = false
) extends SentenceTagger {

  val taggerName = 'verbnetFrame

  override def tag(sentence: Sentence, constraints: Set[TransitionConstraint]): TaggedSentence = {
    val lemmatized = FactorieLemmatizer.tag(sentence, constraints)
    TaggedSentence(
      sentence,
      lemmatized.tags mapValues {
        case tags =>
          val tokLemmaLC = tags.head.value.name
          val tokVerbnetFrames: Set[Symbol] =
            if (useSecondaryFrames) {
              verbnet.getVerbnetFrameSecondaryNames(tokLemmaLC)
            } else {
              verbnet.getVerbnetFramePrimaryNames(tokLemmaLC)
            }
          tokVerbnetFrames map { frame =>
            TokenTag(taggerName, frame)
          }
      }
    )
  }
}
