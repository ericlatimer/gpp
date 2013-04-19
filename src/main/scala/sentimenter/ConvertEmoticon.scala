package sentimenter
import scala.io.Source
import java.io.File

object ConvertEmoticon {


	def main(args: Array[String]){
		val files = new java.io.File(args(0)).listFiles.filter(_.getName.endsWith(".txt"))
		files.foreach{ file => val data = io.Source.fromFile(file.getAbsolutePath()).getLines
		val dataset = <dataset>
{for (line <- data)yield { val information = line.split("\\t")
				val polarity = if (file.getName == "sad.txt") "negative"
						else if (file.getName == "neutral.txt") "neutral"
						else  "positive"    
<item tweetid={information(0).toString} label={polarity.toString} target="unknown" username={information(1).toString}>
	<content>{information(2).toString}</content>
	</item>
}}
</dataset>

		println(dataset)
		}
	}
}
