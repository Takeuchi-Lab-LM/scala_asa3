package cl.asa.init
/**
 * ASAで使用する辞書を指定するクラス
 * 今はここに直書きだが，いい方法があれば変更したい
 */
class YamlFile {
	//val frame: String = "yaml/new_frames2.yaml"
	//val dicframe: String = "yaml/new_frames2.dic"
  
  val frame: String = "yaml/new_argframes.yaml"
  val dicframe: String = "yaml/new_argframes.dic"
  
	val cchart: String = "yaml/ccharts.yaml"
	val diccchart: String = "yaml/ccharts.dic"

	val verb: String = "yaml/verbs.yaml"
	val category: String = "yaml/new_categorys.yaml"

	val idiom: String = "yaml/idioms.yaml"

	val filter: String = "yaml/filters.yaml"
	val dicfilter: String = "yaml/filters.dic"

	val compoundPredicate: String = "yaml/compoundPredicates.yaml"

	val noun: String = "NounTest.yaml"
	def getFrame(): String = {
		return frame
	}
	def getCchart(): String = {
		return cchart
	}
	def getVerb(): String = {
		return verb
	}
	def getCategory(): String = {
		return category
	}
	def getIdiom(): String = {
		return idiom
	}
	def getFilter(): String = {
		return filter
	}
	def getCompoundPredicate(): String = {
		return compoundPredicate
	}
	def getNoun():String ={
		return noun
	}
}