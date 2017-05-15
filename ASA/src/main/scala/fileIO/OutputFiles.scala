package fileIO
import java.io._

class OutputFiles(file: String) {
	val bw = new BufferedWriter(new FileWriter(new File(file)))

	def write(line: String) {
		bw.write(line)
		bw.newLine()
	}

	def close() {
		bw.close()
	}
}