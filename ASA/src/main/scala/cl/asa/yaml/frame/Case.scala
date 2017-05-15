package cl.asa.yaml.frame

class Case {
	var part = ""
	var causative_part = ""
	var passive_part = ""
	var noun = ""
	var category:String = ""
	var semrole = ""
  var arg = ""
	var weight = 0F
	
	def setPart(str:String){
		this.part = str
	}
	def setCausative_part(str:String){
		this.causative_part = str
	}
	def setPassive_part(str:String){
		this.passive_part = str
	}
	def setNoun(str:String){
		this.noun = str
	}
	def setCategory(str:String){
		this.category = str
	}
	def setSemrole(str:String){
		this.semrole = str
	}
  def setArg(str:String){
    this.arg = str
  }
	def setWeight(value:Float){
		this.weight = value
	}
}