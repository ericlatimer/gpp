package sentimenter
import scala.io.Source
import java.io.File

object ConvertEmoticon {

	// Convert the text files in a directory (directory expected as first argument) to the
	// XML format specified on the homework page 5
	def main(args: Array[String]){
		val files = new java.io.File(args(0)).listFiles.filter(_.getName.endsWith(".txt"))
		val dataset = <dataset>
		{
		for(file <- files) yield { 
					{for (line <- io.Source.fromFile(file.getAbsolutePath()).getLines) yield { val information = line.split("\\t")
						val polarity = if (file.getName == "sad.txt") "negative"
						else if (file.getName == "neutral.txt") "neutral"
						else  "positive"    
<item tweetid={information(0).toString} label={polarity.toString} target="unknown" username={information(1).toString}>
	<content>{information(2).toString}</content>
	</item>
}}
		
		}}
</dataset>
	println(dataset)
	}
}
