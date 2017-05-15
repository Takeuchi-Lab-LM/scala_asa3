package cl.asa.yaml.verb
/**
 * 動詞辞書のためのクラス
 * 　動詞辞書の構成
 *  Verbs <-
 *    - [Verb]
 *       -  entry
 *       -  head
 *       -  voice
 */

class Verbs {
	var verb: Array[Verb] = Array.empty

	def setVerb(list: Array[Verb]) {
		this.verb = list
	}
	def isVerb(verbs: String): Boolean = {
		var bool = false
		this.verb.foreach { ver =>
			if (!verbs.isEmpty() && ver.entry.equals(verbs)) {
				println(ver.entry)
				bool = true
			}
		}
		return bool
	}
}