import scala.io.Source
import scala.math._
import cl.asa.yaml._
import cl.asa.init._
import fileIO._
import org.yaml.snakeyaml._
import yamldict._

/**
 * yaml形式の辞書を構築
 */
object createYamlDict {
	//パラメータ
  val infile: String = ""
	val dicttype: String = "vth-arg" //noun, vth, or vth-arg
	//val outfile: String = "yaml/new_Frames2.yaml"
  val outfile: String = "yaml/new_argframes.yaml"

	private val init = new YamlFile
	private val dicts = new LoadYaml(init)
	private val categorys = dicts.getCategorys

	def main(args: Array[String]): Unit = {
		println("-start-")
		val csvs = Source.fromFile(infile).getLines()
		val dict = dicttype match {
			case "noun" =>//名詞項構造データより辞書構築
				val noundict = new NounDict
				noundict.createDict(csvs)
			case "vth" =>//述語項構造シソーラスより辞書構築
				val verbdict = new VerbDict(dicts.getCategorys)
				verbdict.createDict(csvs)
      case "vth-arg" =>
        val verbargdict = new VerbArgDict(dicts.getCategorys)
        verbargdict.createDict(csvs)
		}
		val outputdict = new OutputYamlDict(outfile)
		outputdict.output(dict)
		println("-done-")
	}

}
