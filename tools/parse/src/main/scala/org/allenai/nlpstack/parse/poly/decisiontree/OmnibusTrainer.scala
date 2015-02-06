package org.allenai.nlpstack.parse.poly.decisiontree

class OmnibusTrainer()
    extends ProbabilisticClassifierTrainer {

  val dtTrainer = new DecisionTreeTrainer(0.3)
  val rfTrainer = new OneVersusAllTrainer(new RandomForestTrainer(0, 10, 200))

  override def apply(data: FeatureVectorSource): ProbabilisticClassifier = {
    val trainer = data.classificationTask.filenameFriendlyName match {
      case name if name.startsWith("dt-") =>
        dtTrainer
      case _ =>
        rfTrainer
    }
    trainer(data)
  }
}

