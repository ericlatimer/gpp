package sentimenter

/*
 Copyright 2013 Jason Baldridge
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


/**
 * An example of using the API to classify the prepositional phrase attachment
 * data, trying to be as simple and self-contained as possible.
 *
 * @author jasonbaldridge
 */
object Fancy {

  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear.{LiblinearConfig,Solver}
  import nak.util.ConfusionMatrix
  import chalk.lang.eng.Twokenize

  def apply(trainfile:String,evalfile:String,cost:Double) {
    //Digest training data
    val trainXML = scala.xml.XML.loadFile(trainfile)
    val allTrainingLabels = (trainXML \\ "item").map { item =>
      ((item \ "@label").text)
    }

    val allTrainingTweets = (trainXML \\ "content").map{x => x.text}
    val allTrainingPairs = allTrainingLabels.zip(allTrainingTweets)

    //Digest eval data
    val evalXML = scala.xml.XML.loadFile(evalfile)
    val allEvalLabels = (evalXML \\ "item").map { item =>
      ((item \ "@label").text)
    }
    val allEvalTweets = (evalXML \\ "content").map{x => x.text}
    val allEvalPairs = allEvalLabels.zip(allEvalTweets)

    // A function (with supporting regex) that reads the format of the PPA 
    // files and turns them into Examples. E.g. a line like:
    //   0 join board as director V
    // becames an Example with "V" as the label, and "join board as director"
    // as the features. Normally we'd go ahead and transform this into better
    // features, but this shows what you'd be more likely to do if reading in
    // documents.
    def readRaw(traininPairs: Seq[(String,String)]) = 
      for (pair <- traininPairs)
        yield Example(pair._1, pair._2)

    // A featurizer that simply splits the raw inputs and attaches the
    // appropriate attributes to each of the elements.
    val featurizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokens = Twokenize(input)
        tokens.map{word => FeatureObservation("word"+"="+word)} 
      }
    }

    // Get the training examples in their raw format.  
    val rawExamples = readRaw(allTrainingPairs).toList
    println("rawExamples: " + rawExamples.head)
    
    // Configure and train with liblinear. Here we use the (default) L2-Regularized 
    // Logistic Regression classifier with a C value of .5. We accept the default
    // eps and verbosity values.
    val config = new LiblinearConfig(cost=cost)
    val classifier = LiblinearTrainer.train(config, featurizer, rawExamples)
    
    // Partially apply the labels to the curried 2-arg NakContext.maxLabel function 
    // to create the 1-arg maxLabelPpa function to get the best label for each example.
    def maxLabelPpa = maxLabel(classifier.labels) _

    // Make predictions on the evaluation data. Because the classifier knows about
    // featurization, we can apply the classifier directly to each example using evalRaw.
    val comparisons = for (ex <- readRaw(allEvalPairs).toList) yield 
      (ex.label, maxLabelPpa(classifier.evalRaw(ex.features)), ex.features)

    // Compute and print out the confusion matrix based on the comparisons 
    // obtained above.
    val (goldLabels, predictions, inputs) = comparisons.unzip3
    println(ConfusionMatrix(goldLabels, predictions, inputs))
  }
}