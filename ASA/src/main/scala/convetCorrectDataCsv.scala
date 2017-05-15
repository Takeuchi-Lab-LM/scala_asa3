import fileIO._
import correctdata._
import scala.io._

/**
 * ASAの比較実験をする"compare.scala"で使用できる形式の正解データcsvの生成
 */
object convetCorrectDataCsv {
	//パラメータ
	val infile: String = ""
	val outfile: String = ""
	val itype: String = "vth" //vth or bccwj

	private val Reader = new CsvReader
	private val ReadBCCWJ = new readBCCWJanotate
	private val Extract = new Extraction

	def main(args: Array[String]): Unit = {
		println("-start-")
		itype match {
			case "vth" => convertVth(infile, outfile)
			case "bccwj" => convertBCCWJ(infile, outfile)
			case _ =>
		}
		println("-done-")
	}

	def convertVth(infile: String, outfile: String) {
		val orignal = Source.fromFile(infile).getLines.map(Reader.parseLine(_).get).toList
		val csvs = Extract.extractVth(orignal.tail)
		new CsvWriter(outfile).output(csvs)
	}

	def convertBCCWJ(infile: String, outfile: String) {
		val orignal = ReadBCCWJ.bccwj(infile)
		val csvs = Extract.extractBCCWJ(orignal)
		new CsvWriter(outfile).output(csvs)
	}
}
