package cl.asa.result
/**
 * 形態素の解析結果格納用
 */

class Morph(m_id: Int, line: String, analyzer: String) {
	var id: Int = 0 //形態素のid
	var surface: String = "" //形態素の表層
	var pos: String = "" //品詞，品詞細分類1，品詞細分類2，品詞細分類3
	var pos1 = ""
	var pos2 = ""
	var pos3 = ""
	var pos4 = ""
	var base: String = "" //基本形
	var read: String = "" //読み
	var cform: String = "" //活用形
	var ctype: String = "" //活用型
	var ne: String = "" //固有表現解析
	var tree = Seq.empty[Morph]
	var chunk: Chunk = null
	var forms: Array[String] = null

	analyzer match {
      case "cabocha -n1 -f1" => initMorph()
      case "jdepp" => initMorphjdepp()
    }

	def initMorph() {
		val div1: Array[String] = line.split('\t')
		val div2: Array[String] = div1(1).split(',').padTo(9, "")
		id = m_id
		surface = div1(0)
		if (!div2(0).equals("*")) { pos1 = div2(0) }
		if (!div2(1).equals("*")) { pos2 = div2(1) }
		if (!div2(2).equals("*")) { pos3 = div2(2) }
		if (!div2(3).equals("*")) { pos4 = div2(3) }
		if (!div2(4).equals("*")) { cform = div2(4) }
		if (!div2(5).equals("*")) { ctype = div2(5) }
		pos = getPos()
		base = div2.apply(6)
		read = div2.apply(7)
		ne = div1.apply(2)
	}

	def initMorphjdepp() {
		val div1: Array[String] = line.split('\t')
		val div2: Array[String] = div1(1).split(',').padTo(9, "")
		id = m_id
		surface = div1(0)
		if (!div2(0).equals("*")) { pos1 = div2(0) }
		if (!div2(1).equals("*")) { pos2 = div2(1) }
		if (!div2(2).equals("*")) { cform = div2(2) }
		if (!div2(3).equals("*")) { ctype = div2(3) }
		pos = getPosjdepp()
		base = div2.apply(4)
		read = div2.apply(5)
		//ne = div1.apply(2)
	}

	private def getPos(): String = {
		var pos: String = ""
		if (!pos1.isEmpty()) { pos = pos + pos1 }
		if (!pos2.isEmpty()) { pos = pos + "," + pos2 }
		if (!pos3.isEmpty()) { pos = pos + "," + pos3 }
		if (!pos4.isEmpty()) { pos = pos + "," + pos4 }
		return pos
	}

	private def getPosjdepp(): String = {
		var pos: String = ""
		if (!pos1.isEmpty()) { pos = pos + pos1 }
		if (!pos2.isEmpty()) { pos = pos + "," + pos2 }
		return pos
	}
}
