package cl.asa.yaml.filter

class Filter {
	var entry: String = ""
	var negative = new Feature
	var positive = new Feature

	def setEntry(str:String){
		this.entry = str
	}
	def setNegative(feature:Feature){
		this.negative = feature
	}
	def setPositive(feature:Feature){
		this.positive = feature
	}
}