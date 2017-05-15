package cl.asa.yaml.category
/**
 * カテゴリー辞書のためのクラス
 * 　カテゴリ辞書の構成
 *  Categorys
 *    - [Category]<-
 *       - [noun]
 *       -  category_name
 */

class Category {
	var category_name: String = null
	var noun:Array[String] = Array.empty

	def setCategory_name(str: String) {
		this.category_name = str
	}

	def setNoun(list: Array[String]) {
		this.noun = list
	}
}