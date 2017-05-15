package fileIO
import java.io._

class CsvWriter(file: String) {
	val bw = new BufferedWriter(new FileWriter(new File(file)))

	def output(csvs: List[List[String]]) {
		csvs.foreach { csv =>
			write(csv.mkString("\"", "\",\"", "\""))
		}
		this.fclose
	}
	private def write(line: String) {
		bw.write(line)
		bw.newLine()
	}

	private def fclose() {
		bw.close()
	}
}