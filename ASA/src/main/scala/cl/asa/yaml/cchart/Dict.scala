package cl.asa.yaml.cchart

class Dict {
	var dict:Array[Cchart] = Array.empty 
	
	def getCchart(ctype: String): Option[Cchart] = {
		val cchart = dict.find { cchart =>
			cchart.ctype.equals(ctype)
		}
		return cchart
	}
	
	def setDict(list:Array[Cchart]){
		this.dict = list
	}
}