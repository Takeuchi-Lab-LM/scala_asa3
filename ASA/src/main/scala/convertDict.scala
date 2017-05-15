import scala.xml.XML
import fileIO._
import toyaml._

/**
 * perl版ASAのXML辞書をscala版ASAのYAML辞書に変換
 * 以下の辞書の変換に対応(今のところ竹内研の10種類辞書のみに対応)
 * - 活用型辞書(cchart)
 * - 語義辞書(frame)
 * - 名詞カテゴリ辞書(category)
 * - 慣用句辞書(idiom)
 * - 慣用句フィルタ辞書(filter)
 * 
 * @param xml_file 変換するXMLファイル名
 * @param yaml_file 生成するファイル名
 * @param dict_type 変換する辞書タイプの指定(cchart, frame, category, idiom, filter)
 * 
 * @todo 今はプログラム内に直書きしているので，プログラム起動時の引数で指定するようにする予定
 */

object convertDict {
	def main(args: Array[String]): Unit = {
		var infile:String = ""
		var outfile:String = "filters.yaml"
		var itype:String = "filter"
		
		val xml = XML.loadFile(infile)
		val file = new OutputFiles(outfile)
		
		itype match {
			case "idiom" => new toyaml.ToIdiomYaml(xml, file).convert
			case "cchart" => new toyaml.ToCchartYaml(xml, file).convert
			case "category" => new toyaml.ToCategoryYaml(xml, file).convert
			case "frame" => new toyaml.ToFrameYaml(xml, file).convert
			case "filter" => new toyaml.ToFilterYaml(xml, file).convert
			case _ => println("not type")
		}
		file.close
		println("done")
	}
}
