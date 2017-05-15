package cl.asa.yaml.verb
/**
 * 動詞辞書のためのクラス
 * 　動詞辞書の構成
 *  Verbs 
 *    - [Verb] <-
 *       -  entry
 *       -  head
 *       -  voice
 */

class Verb {
	var entry: String = ""
	var head: String = ""
	var voice: String = ""
		
	def setEntry(str:String){
		this.entry = str
	}
	def setHead(str:String){
		this.head = str
	}
	def setVoice(str:String){
		this.voice = str
	}
}