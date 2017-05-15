package cl.asa.parse
import cl.asa.parse.cabocha._
import cl.asa.result._

class CabochaParse {
	private val cabocha = new Cabocha("cabocha -f1 -n1", "utf-8")
	private val basic = new Basic()
	
	def parse(line: String): Result = {
		var result = cabocha.parse(line)
		result = basic.parse(result)
		return result
	}
	
	def end() {
		cabocha.end
	}
}