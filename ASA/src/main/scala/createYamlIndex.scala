import scala.io.Source
import fileIO._

/**
 * Yaml辞書よりIndex辞書を構築
 */
object createYamlIndex {
	//パラメータ
	val infile: String = "yaml/new_argframes.yaml"
	val outfile: String = "yaml/new_argframes.dic"

	def main(args: Array[String]): Unit = {
		println("-start-")
		val yaml = Source.fromFile(infile).getLines
		val index = this.createIndex(yaml)
		this.OutputIndex(index, outfile)
		println("-done-")
	}

	/**
	 * Dictクラス直下の要素ごとに開始地点と終了地点のファイルポインタを保存
	 * 要素ごとに開始地点のみを保存した配列(ary)を作成した後、次の要素の開始地点を終了地点としたMap(index)を作成
	 */
	def createIndex(yaml: Iterator[String]): Array[(String, Int, Int)] = {
		var ary: Array[(String, Int)] = Array.empty
		var fp: Int = 0
		yaml.foreach { str =>
			str.startsWith("- ") match {
				case true => ary = ary :+ (str.split(": ")(1), fp)
				case false =>
			}
			fp = fp + str.getBytes().size + 1
		}
		ary = ary :+ ("end", fp)
		val index: Array[(String, Int, Int)] = ary.sliding(2).toArray.map { n =>
			(n(0)._1, n(0)._2, n(1)._2)
		}
		return index
	}

	def OutputIndex(index: Array[(String, Int, Int)], outfile: String) {
		val file = new OutputFiles(outfile)
		index.foreach(in => file.write(in._1 + " " + in._2 + " " + in._3))
		file.close

	}
}