package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.ml.FeatureVector

/** A SculptureFeature computes a feature vector corresponding to a given sculpture. */
abstract class SculptureFeature extends (Sculpture => FeatureVector)
