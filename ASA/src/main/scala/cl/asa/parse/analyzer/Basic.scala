package cl.asa.parse.analyzer
/**
 *   cabochaの解析結果より文節間の関係や，語義付与に必要な動詞や格助詞などの情報の付与するクラス
 */
import cl.asa.result._
import cl.asa.yaml.frame._

class Basic() {
	var frames: Dict2 = null

	def setFrames(frames: Dict2) {
		this.frames = frames
	}

	def parse(result: Result): Result = {
		result.chunks.foreach { chunk =>
			chunk.surface = this.getChunkSurface(chunk)
			chunk.modifyingchunk = this.getModifyingChunk(result, chunk)
			chunk.modifiedchunks = this.getModifiedChunks(result, chunk)
			chunk.ctype = this.getChunkType(chunk)
			chunk.main = this.getHead(chunk)
			chunk.part = this.getPart(chunk)
			chunk.morphs.foreach(_.chunk = chunk)
		}
		return result
	}

	/**
	 * 文節内の形態素の表層をつなげて，文節の表層を取得
	 * @param chunk 文節
	 * @return 文節の表層
	 */
	private def getChunkSurface(chunk: Chunk): String = {
		val surface: String = chunk.morphs.map(_.surface).mkString
		return surface
	}

	/**
	 * 係っている文節を取得
	 */
	private def getModifyingChunk(result: Result, chunk: Chunk): Chunk = {
		val modifyingchunk: Chunk = chunk.link match {
			case -1 => null
			case _ => result.chunks(chunk.link)
		}
		return modifyingchunk
	}

	/**
	 * 文節の係りを受けている文節を取得
	 * @param chunk 文節
	 * @return 係り元の文節集合
	 */
	private def getModifiedChunks(result: Result, depchunk: Chunk): Seq[Chunk] = {
		val linkchunks: Seq[Chunk] = result.chunks.filter { chunk =>
			chunk.link.equals(depchunk.id)
		}
		return linkchunks
	}

	/**
	 * 文節のタイプを取得
	 *  - verb:      動詞
	 *  - adjective: 形容詞，形容動詞
	 *  - copula:    コピュラ(AはBだ)
	 *  - elem:      その他
	 */
	private def getChunkType(chunk: Chunk): String = {
		var ctype: String = chunk.morphs match {
			case morphs if morphs.exists { morph =>
				morph.pos.contains("動詞,自立")
			} => "verb"
			case morphs if morphs.exists { morph =>
				morph.pos.contains("形容詞,自立") | morph.pos.contains("形容動詞語幹")
			} => "adjective"
			case morphs if morphs.exists { morph =>
				morph.cform.contains("特殊・ダ") | morph.cform.contains("特殊・デス")
			} => "copula"
			case _ => "elem"
		}
		return ctype
	}

	/**
	 * その文節の主辞となるような語の取得(意味役割付与に使用)
	 */
	private def getHead(chunk: Chunk): String = {
		val head: String = chunk.ctype match {
			case "copula" =>
				chunk.morphs.withFilter { morph =>
					morph.pos.contains("名詞")
				}.map { morph =>
					morph.base.equals("*") match {
						case true => morph.surface
						case false => morph.base
					}
				}.mkString
			case "verb" =>
				chunk.morphs.find(_.base.equals("する")) match {
					case Some(morph) =>
						val sahen = chunk.morphs.filter(_.id < morph.id).map(_.surface)
						sahen.nonEmpty match {
							case true =>
								frames.ne(null) match {
									case true =>
										val predicate = sahen.slice(sahen.length - 2, sahen.length).mkString + "する"
										frames.isFrame(predicate) match {
											case true => predicate
											case false => sahen.last + "する"
										}
									case false => sahen.last + "する"
								}
							case false => "する"
						}
					case None =>
						val morph = chunk.morphs.find(_.pos.equals("動詞,自立")).get
						frames.ne(null) match {
							case true =>
								val predicate = chunk.morphs.find(m => m.pos1.equals("動詞") && m.id.equals(morph.id + 1)) match {
									case Some(m) => morph.surface + m.base
									case None =>
										chunk.morphs.find(_.id.equals(morph.id - 1)) match {
											case Some(m) => m.surface + morph.base
											case None => morph.base
										}
								}
								frames.isFrame(predicate) match {
									case true => predicate
									case false => morph.base
								}
							case false => morph.base
						}
				}
			case "adjective" =>
				chunk.morphs.filter { morph =>
					morph.pos.contains("形容詞,自立") | morph.pos.contains("形容動詞語幹")
				}.head match {
					case morph if morph.pos.contains("形容詞,自立") => morph.base
					case morph if morph.pos.contains("形容動詞語幹") =>
						chunk.morphs.find(_.id.equals(morph.id - 1)) match {
							case Some(premorph) => premorph.surface + morph.base + "だ"
							case None => morph.base + "だ"
						}
				}
			case "elem" =>
				chunk.morphs.withFilter { morph =>
					morph.pos.contains("名詞") || morph.pos.contains("副詞")
				}.map(_.surface).mkString
		}
		return head
	}

	/**
	 * 文節内の名詞につく格助詞の取得
	 * @param chunk 文節
	 * @return 文節内の格助詞or係助詞
	 */
	private def getPart(chunk: Chunk): String = {

		val part: String = chunk.morphs.filter { morph =>
			morph.pos.contains("格助詞") | morph.pos.contains("係助詞") | morph.pos2.equals("連体化") | morph.pos.contains("助動詞")
		}.lastOption match {
			case Some(morph) => morph.base
			case None => ""
		}
		return part
	}
}
