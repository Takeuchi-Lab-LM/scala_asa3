package cl.asa.yaml.filter

class Dict {
	var dict:Array[Filter] = Array.empty

	def getFilter(entry: String): Option[Filter] = {
		val filters = dict.find { filter =>
			filter.entry.equals(entry)
		}
		return filters
	}
	
	def setDict(list:Array[Filter]){
		this.dict = list
	}
}