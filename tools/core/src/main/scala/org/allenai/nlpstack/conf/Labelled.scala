package org.allenai.nlpstack.conf

/** A representation of a labelled extraction.
  *
  * @param  label  whether this extraction is true or false
  * @param  item  the item labelled
  */
case class Labelled[E](label: Boolean, item: E)