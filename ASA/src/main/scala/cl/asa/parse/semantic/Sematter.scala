package cl.asa.parse.semantic
/**
 * 語義，意味役割を付与するためのクラス
 */
import cl.asa.result._
import cl.asa.yaml.frame._
import cl.asa.yaml.category._
import cl.asa.yaml.verb._
import cl.asa.yaml._

class Sematter(frames: frame.Dict2, categorys: category.Dict, nouns: noun.Dict) {
	private val calc = new Calculate(frames)
	private val adjunct = new Adjunct()
	private val nounstruct = new NounStructure(nouns, frames)

	def parse(result: Result): Result = {
		val verbchunks: Seq[Chunk] = getSemChunks(result)
		verbchunks.foreach { verbchunk =>
			val linkchunks = this.getLinkChunks(verbchunk)
			this.setAnotherPart(linkchunks)
			calc.getFrame(verbchunk.main, linkchunks) match {
				case Some((semantic, similar, insts)) =>
					this.setSemantic(semantic, similar, verbchunk)
					this.setSemRole(insts)
          this.setArg(insts)
					this.setSpecialSemantic(verbchunk)
					adjunct.parse(verbchunk.modifiedchunks)
				case None =>
			}
		}
		val nounchunks: Seq[Chunk] = getNounChunks(result)
		nounchunks.foreach { nounchunk =>
			nounstruct.parse(nounchunk)
		}
    setInversedSemantic(result)
		return result
	}
  
  def setInversedSemantic(result:Result){
    result.chunks.foreach(getModChunk)
  }
  
  private def getModChunk(chunk: Chunk): Unit = {
    val chunks = chunk.modifyingchunk.ne(null) match {
      case true => chunk.modifiedchunks :+ chunk.modifyingchunk
      case false => chunk.modifiedchunks
    }
    chunk.modifiedchunks = chunks
  }

	def setSpecialSemantic(chunk: Chunk) {
		chunk.semantic.split("-").padTo(5, "") match {
			case semantics if semantics(1).equals("位置変化") & semantics(3).equals("着点への移動") =>
				chunk.modifiedchunks.exists(_.semrole.contains("対象")) match {
					case true =>
					case false =>
						chunk.modifiedchunks.find(_.part.equals("が")) match {
							case Some(schunk) => schunk.semrole = schunk.semrole :+ "対象"
							case None =>
						}
				}
			case semantics if semantics(1).equals("位置変化") & semantics(2).equals("位置変化（物理）（人物間）") & semantics(3).equals("他者からの所有物の移動") =>
				chunk.modifiedchunks.exists(_.semrole.contains("着点")) match {
					case true =>
					case false =>
						chunk.modifiedchunks.find(c => c.semrole.contains("動作主") | c.semrole.contains("経験者")) match {
							case Some(schunk) => schunk.semrole = schunk.semrole :+ "着点"
							case None =>
						}

				}
			case _ =>
		}
	}

	/**
	 * 助詞の言い換え候補があるものに対して，言い換えの助詞を付与
	 */
	private def setAnotherPart(chunks: Seq[Chunk]) {
		chunks.foreach { chunk =>
			chunk.morphs.foreach { morph =>
				morph.pos match {
					case pos if pos.contains("格助詞") && Seq("に", "へ").contains(chunk.part) =>
						chunk.another_parts = Seq("に", "へ").diff(chunk.part)
					case pos if pos.contains("係助詞") =>
						chunk.another_parts = Seq("が", "を")
					case _ => null
				}
			}
		}
	}

	def getNounChunks(result: Result): Seq[Chunk] = {
		val chunks: Seq[Chunk] = result.chunks.filter { chunk =>
			nouns.isFrame(chunk.main)
		}
		return chunks
	}

	/**
	 * 係り先である節を取得
	 */
	def getSemChunks(result: Result): Seq[Chunk] = {
		val chunks: Seq[Chunk] = result.chunks.filter { chunk =>
			!chunk.ctype.equals("elem") && frames.isFrame(chunk.main)
		}
		return chunks
	}
	/**
	 * 係り先の節を渡して，その係り元を取得
	 */
	private def getLinkChunks(verbchunk: Chunk): Seq[Chunk] = {
		val linkchunks = 
			verbchunk.modifyingchunk.ne(null) match{
			case true if verbchunk.modifyingchunk.ctype.equals("elem") =>
				val can = verbchunk.modifiedchunks.map(_.part)
				verbchunk.modifyingchunk.another_parts  = Seq("が","を","に").diff(can)
				verbchunk.modifiedchunks :+ verbchunk.modifyingchunk
			case _ => verbchunk.modifiedchunks
		}
		return linkchunks
	}

	/**
	 * 曖昧性を解消したフレームのデータより語義を付与
	 * @param semantic Calcurateクラスより取得したデータ
	 * @param verbchunk 語義を付与する文節
	 */
	private def setSemantic(semantic: String, similar: Float, verbchunk: Chunk) {
		verbchunk.semantic = semantic
		verbchunk.similar = similar
	}

	/**
	 * 曖昧性を解消したフレームのデータより意味役割を付与
	 * @param semantic Calcurateクラスより取得したデータ
	 */
	private def setSemRole(insts: Seq[(Float, Case, Chunk)]) {
		insts.foreach { instset =>
			val (similar, icase, chunk) = instset
			if (icase.semrole.ne(null)) chunk.semrole = chunk.semrole :+ icase.semrole
			chunk.similar = similar
			chunk.category.contains(icase.category) match {
				case true => chunk.category = (icase.category +: chunk.category).distinct
				case false =>
			}
		}
	}
  
  private def setArg(insts: Seq[(Float, Case, Chunk)]) {
    insts.foreach { instset =>
      val (similar, icase, chunk) = instset
      if (icase.arg.ne(null)) chunk.arg = chunk.arg :+ icase.arg
      chunk.similar = similar
      chunk.category.contains(icase.category) match {
        case true => chunk.category = (icase.category +: chunk.category).distinct
        case false =>
      }
    }
  }

}
