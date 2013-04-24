package sentimenter

import nak.util.ConfusionMatrix
import chalk.lang.eng.PorterStemmer
import chalk.util.SimpleTokenizer

/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 *	-c, --cost  <arg>       The cost parameter C. Bigger values means less
 *                          regularization (more fidelity to the training set).
 *                          (default = 1.0)
 *  -d, --detailed          
 *  -e, --eval  <arg>...    The files containing evalualation events.
 *  -x, --extended          Use extended features.
 *  -m, --method  <arg>     The type of solver to use. Possible values: majority,
 *                          lexicon, or any liblinear solver type.
 *                         (default = L2R_LR)
 *  -t, --train  <arg>...   The files containing training events.
 *  -v, --verbose           
 *      --help              Show this message
 *      --version           Show version of this program
 *
 */
object SentimenterOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
	val cost = opt[Double]("cost", short = 'c', default=Some(1.0), descr = "The cost parameter C.")
	val detailed = opt[Boolean]("detailed", short = 'd', descr = "Should output the correctly and incorrectly results please.")
	val eval = opt[List[String]]("eval", short = 'e', descr = "The files containing evalualation events.")
	val extended = opt[Boolean]("extended", short = 'x', default=Some(false), descr = "Use extended features.")
	val method = opt[String]("method", short = 'm', default=Some("L2R_LR"), descr = "The type of solver to use. Possible values: majority, lexicon, or any liblinear solver type.")
	val train = opt[List[String]]("train", short = 't', descr = "The files containing training events.")
	val verbose = opt[Boolean]("verbose", short = 'v', default=Some(false), descr = "Use extended features.")
	val help = opt[Boolean]("help", noshort = true, descr = "Show this message.")
	val version = opt[Boolean]("version", noshort = true, default=Some(false), descr = "Show version of this program.")
  }
}

/**
 * 
 */
object Sentimenter {

  /**
   * Main that processes the command line options and 
   * initiates the selected kind of classification:
   * Majority -> majority class baseline (Majority.scala)
   * Lexicon -> lexicon ratio baseline  (Lexicon.scala)
   * Fancy -> L2R_LR trained, use --extended for extended features (Fancy.scala)
   * with the indicated training and evaluation files.
   */
  def main(args: Array[String]) {
	val opts = SentimenterOpts(args)

  if (opts.version()) {
    println("(ELAF) Sentimenter Version 0.1.1")
    System.exit(0)
  }

  if (opts.verbose())
    println("Verbose mode enabled.")

	val trainFile = if (opts.train().length == 1) opts.train().head 
					else getSingleFile(opts.train(),"trainFile.xml")
	val evalFile = if (opts.eval().length == 1) opts.eval().head 
					else getSingleFile(opts.eval(),"evalFile.xml")					

	if (opts.method() == "majority") {
		Majority(trainFile, evalFile, opts.detailed())
	} else if (opts.method() == "lexicon") {
		Lexicon(evalFile, opts.detailed())
	} else
		Fancy(trainFile, evalFile, opts.cost(), opts.extended(), opts.detailed())
  }


  // If multiple files are specified for training and/or evaluation data, create a single file in
  // the appropriate XML format.  
  def getSingleFile(fileList:List[String],fileName:String) = {
  	val out = new java.io.FileWriter(fileName)
  	out.write("<?xml version=\"1.0\"?>\n")
  	out.write("<dataset>\n")
  	for (file <- fileList) {
  		val lines = scala.io.Source.fromFile(file).getLines
  		for (line <- lines) {
  			if (!line.startsWith("<?xml version") && !line.startsWith("<dataset") && !line.startsWith("</dataset"))
  				out.write(line+"\n")			
  		}
  	}
  	out.write("</dataset>")
	out.close

  	fileName
  }

}