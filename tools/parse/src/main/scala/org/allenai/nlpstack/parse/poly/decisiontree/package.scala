package org.allenai.nlpstack.parse.poly

/** Implements C4.5 decision trees for integral labels and attributes.
  *
  * Main class to use is [[org.allenai.nlpstack.parse.poly.decisiontree.DecisionTree]].
  * Use the companion object to build the tree.
  * Then use [[org.allenai.nlpstack.parse.poly.decisiontree.DecisionTree.classify( )]]
  * or
  * [[org.allenai.nlpstack.parse.poly.decisiontree.DecisionTree.distributionForInstance( )]]
  * to do prediction.
  *
  * The tree takes data in the form of
  * [[org.allenai.nlpstack.parse.poly.decisiontree.FeatureVectors]].
  * This is a container for a collection of
  * [[org.allenai.nlpstack.parse.poly.decisiontree.FeatureVector]] objects.
  *
  * Implementations of these are
  * [[org.allenai.nlpstack.parse.poly.decisiontree.SparseVector]]
  * or
  * [[org.allenai.nlpstack.parse.poly.decisiontree.DenseVector]].
  */
package object decisiontree {

}
