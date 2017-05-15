package toyaml
import fileIO._
/**
 * XMLの慣用句フィルタ辞書(filter)をYAMLに変換
 * @param xml 読み込んだXMLファイル
 * @param file　生成するファイルのためのOutputFilesクラス
 */
class ToFilterYaml(xml: scala.xml.Elem, file: OutputFiles) {
	val tab = " " * 2
	def convert() {
		file.write("---")
		file.write("dict:")
		xml.\("filter").foreach(convertFilter(_))
	}

	private def convertFilter(filter: scala.xml.Node) {
		val indent = tab * 0
		file.write(indent + "- " + "entry: " + filter.\("idiom_entry").text)
		filter.\("feature").foreach { feature =>
			feature.\("@flag").text match {
				case "I" =>
					file.write(indent + tab + "positive: ")
					convertFeature(feature)
				case "L" =>
					file.write(indent + tab + "negative: ")
					convertFeature(feature)
			}
		}
	}

	private def convertFeature(feature: scala.xml.Node) {
		val indent = tab * 1
		feature.\("idiom_polarity") match {
			case polarity if polarity.nonEmpty => file.write(indent + tab + "polarity: " + polarity.text)
			case _ => None
		}
		feature.\("modifier_semattr_category") match {
			case categorys if categorys.nonEmpty =>
				file.write(indent + tab + "category: ")
				categorys.foreach { category =>
					file.write(indent + tab + "- " + category.text)
				}
			case _ => None
		}
		feature.\("modifier_sentelem") match {
			case sentelems if sentelems.nonEmpty =>
				file.write(indent + tab + "sentelem: ")
				sentelems.foreach { sentelem =>
					file.write(indent + tab + "- " + sentelem.text)
				}
			case _ => None
		}
		feature.\("idiom_mood") match {
			case moods if moods.nonEmpty =>
				file.write(indent + tab + "mood: ")
				moods.foreach { mood =>
					file.write(indent + tab + "- " + mood.text)
				}
			case _ => None
		}
		feature.\("idiom_voice") match {
			case voices if voices.nonEmpty =>
				file.write(indent + tab + "voice: ")
				voices.foreach { voice =>
					file.write(indent + tab + "- " + voice.text)
				}
			case _ => None
		}
	}
}