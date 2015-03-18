package org.allenai.nlpstack.core.conf

import org.allenai.common.Resource.using

import java.io.{ BufferedOutputStream, File, FileOutputStream, OutputStream }

/** A confidence function for ranking how likely an extraction is correct.
  *
  * @tparam  E  the extraction to rank
  * @param  featureSet  the features to use
  */
abstract class ConfidenceFunction[E](val featureSet: FeatureSet[E, Double])
    extends Function[E, Double] {
  def apply(that: E): Double

  def save(output: OutputStream): Unit
  def saveFile(file: File) {
    using(new BufferedOutputStream(new FileOutputStream(file))) { stream =>
      this.save(stream)
    }
  }
}
