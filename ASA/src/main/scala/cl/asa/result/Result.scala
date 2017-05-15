package cl.asa.result

class Result(line:String = null) {
	var chunks: Seq[Chunk] = Seq.empty[Chunk] //文中の文節
	var surface: String = line //文の表層

	def addChunk(chunk: Chunk) {
		chunks = chunks :+ chunk
	}
}