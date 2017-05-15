package cl.asa.yaml.filter

class Feature {
	var polarity: String = ""
	var category: Array[String] = Array.empty
	var sentelem: Array[String] = Array.empty
	var voice: Array[String] = Array.empty
	var mood: Array[String] = Array.empty

	def setPolarity(str: String) {
		this.polarity = str
	}
	def setCategory(list: Array[String]) {
		this.category = list
	}
	def setSentelem(list:Array[String]){
		this.sentelem = list
	}
	def setVoice(list:Array[String]){
		this.voice = list
	}
	def setMood(list:Array[String]){
		this.mood = list
	}
}