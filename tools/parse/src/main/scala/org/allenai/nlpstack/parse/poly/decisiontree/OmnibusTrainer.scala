package org.allenai.nlpstack.parse.poly.decisiontree

class OmnibusTrainer()
    extends ProbabilisticClassifierTrainer {

  val dtTrainer = new RandomForestTrainer(0, 30, 200)
  //new DecisionTreeTrainer(0.3)
  val rfTrainer = new RandomForestTrainer(0, 30, 200)
  //new OneVersusAllTrainer(new RandomForestTrainer(0, 10, 400))

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

