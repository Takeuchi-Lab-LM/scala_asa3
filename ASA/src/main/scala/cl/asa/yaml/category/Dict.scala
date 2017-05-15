package cl.asa.yaml.category

class Dict {
	//@BeanProperty var dict = new java.util.ArrayList[Category]()
	var dict: Array[Category] = Array.empty

	def getCate1(noun: String): String = {
		val category_name = dict.find { category =>
			category.noun.contains(noun)
		} match {
			case Some(category) => category.category_name
			case None => ""
		}
		return category_name
	}

	def getCates(noun: String): Array[String] = {
		val category_names:Array[String] = dict.collect {
			case category if category.noun.contains(noun) =>
			category.category_name
		}
		return category_names.distinct
	}

	def setDict(list: Array[Category]) {
		this.dict = list
	}
}