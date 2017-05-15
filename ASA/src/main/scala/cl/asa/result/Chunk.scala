package cl.asa.result
/**
 * 文節の情報を格納するクラス(構造体)
 */

class Chunk(line: String = "") {
	// 必須な基本情報
	var id: Int = 0 //文節のid
	var surface: String = "" //文節の表層
	var morphs: Seq[Morph] = Seq.empty[Morph] //文節内の形態素
	var modifyingchunk: Chunk = null //係りの文節
	var modifiedchunks: Seq[Chunk] = Seq.empty[Chunk] //受けの文節

	// あんまりいらない情報(?)
	var link: Int = 0 //係り先
	var head: Int = 0 //主要語
	var fanc: Int = 0 //機能語
	var score: Float = 0F //係り関係のスコア
	
	// 整理により付与する情報
	var main: String = ""
	var ctype: String = ""
	var verb: String = ""
	var part: String = "" //名詞につく格の情報

	//態などの情報
	var tense: String = ""
	var voice: String = ""
	var polarity: String = ""
	var sentelem: String = ""
	var mood: String = ""

	//語義や意味役割に必要な変数
	var semantic: String = ""
	var semrole: Seq[String] = Seq.empty
  var arg: Seq[String] = Nil
	var category: Array[String] = Array.empty
	var adjunct: String = ""
	var similar: Float = 0F
	var another_parts: Seq[String] = Seq.empty

	var idiom: String = ""
	var phrase: Seq[String] = Seq.empty
	var idiom_morph: Seq[Morph] = Seq.empty
	var idiom_score: Float = 0F
	
	var noun_agentiveL:String = ""
	var noun_semantic:String = ""
	var noun_semrole:String = ""
	var noun_arg:String = ""
	var noun_agentiveRole:String = ""
		
	
	
	initChunk()
	private def initChunk() {
		val div1: Array[String] = line.split(" ")
		val div2: Array[String] = div1.apply(3).split("/")
		id = div1.apply(1).toInt
		link = div1.apply(2).replace("D", "").toInt
		head = div2.apply(0).toInt
		fanc = div2.apply(1).toInt
		score = div1.apply(4).toFloat
	}

	
	def addMorph(morph: Morph) {
		morphs = morphs :+ morph
	}
}