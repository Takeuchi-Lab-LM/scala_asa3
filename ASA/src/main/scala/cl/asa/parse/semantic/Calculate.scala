package cl.asa.parse.semantic
/**
 * フレームより曖昧性を解消する計算を行うクラス
 *
 * @todo ここで求まる結果はタプルで無理やり格納して返している　＜ー新たな構造体などを用意してきれいにしたい
 */
import cl.asa.yaml.frame._
import cl.asa.result._
import java.util.ArrayList
import scala.collection.JavaConversions._

class Calculate(frames: Dict2) {
	/**
	 * 述語のフレームを取得し，その内から最も類似度の高いフレームを取得
	 */
	def getFrame(verb: String, linkchunks: Seq[Chunk]): Option[(String, Float, Seq[(Float, Case, Chunk)])] = {
		val frameset = frames.getFrame(verb) match {
			case Some(frames) =>
				frames.frame.flatMap { frame =>
					frame.instance.ne(null) match {
						case true =>
							frame.instance.map { instance =>
								val (similar, insts) = calculateSntSimilar(instance, linkchunks)
								(frame.semantic, similar, insts)
							}
						case false => Seq((frame.semantic, -1.0F, Seq.empty))
					}
				}.maxBy(_._2)
			case None => null
		}
		return Option(frameset)
	}

	/**
	 * 事例の類似度を算出
	 */
	def calculateSntSimilar(instance: Instance, linkchunks: Seq[Chunk]): (Float, Seq[(Float, Case, Chunk)]) = {
		var comb: Seq[(Float, Case, Chunk)] = calculateAllCombinations(instance, linkchunks)
		var insts: Seq[(Float, Case, Chunk)] = Seq.empty
		while (comb.exists(_._1 > 0)) {
			val max = comb.maxBy(pairs => pairs._1)
			insts = insts :+ max
			comb = comb.filterNot { arg =>
				(arg._2.noun + arg._2.part).equals(max._2.noun + max._2.part) || arg._3.eq(max._3)
			}
		}
		val similar = insts.map(_._1).sum
		return (similar, insts)
	}

	/**
	 * 入力文と事例の項のすべての組み合わせの項類似度を求める
	 */
	def calculateAllCombinations(instance: Instance, linkchunks: Seq[Chunk]): Seq[(Float, Case, Chunk)] = {
		val combinations = linkchunks.flatMap { linkchunk =>
			instance.cases.map { icase =>
				val similar = calculateArgSimilar(icase, linkchunk)
				(similar, icase, linkchunk)
			}
		}
		return combinations
	}
	/**
	 * 項類似度を算出
	 *
	 */
	private def calculateArgSimilar(icase: Case, chunk: Chunk): Float = {
		val nounsimilar: Float = getNounSimilar(icase, chunk)
		val partsimilar: Float = getPartSimilar(icase, chunk)
		val surfsimilar: Float = getSurfSimilar(icase, chunk)
		val similar: Float = partsimilar * (surfsimilar + partsimilar + nounsimilar) * icase.weight
		//println(icase.noun+"("+icase.category+")" +": "+ chunk.main + "= " + similar)
		return similar
	}

	/**
	 * 名詞のカテゴリーによる類似度
	 */
	private def getNounSimilar(icase: Case, chunk: Chunk): Float = {
		val similar: Float = chunk.category.contains(icase.category) match {
			case true => 1.0F
			case false => 0.0F
		}
		return similar
	}
	/*
	 * 起動の時に引数にファイル名があれば，ファイルの内容を解析する
	 */

	/**
	 * 　名詞の表層による類似度
	 */
	private def getSurfSimilar(icase: Case, chunk: Chunk): Float = {
		val similar: Float = chunk.main match {
			case surf if surf.isEmpty => 0.0F
			case surf if surf.equals(icase.noun) => 1.0F
			case _ => 0.0F
		}
		return similar
	}

	/**
	 * 名詞につく格による類似度
	 */
	private def getPartSimilar(icase: Case, chunk: Chunk): Float = {
		val similar: Float = icase.part.ne(null) match {
			case true => icase.part match {
				case part if chunk.part.equals(part) => 1.0F
				case part if part.equals("は") && chunk.part.equals("が") => 1.1F //シソーラスの格が"は"のとき"が" 
				case part if part.equals("は") && chunk.part.equals("を") => 1.1F //シソーラスの格が"は"のとき"を"
				case part if chunk.another_parts.contains(part) => 1.0F
				case _ if chunk.modifyingchunk.ne(null) =>
					chunk.modifyingchunk.voice match {
						case "CAUSATIVE" if chunk.part.equals(icase.causative_part) => 1.0F
						case "PASSIVE" if chunk.part.equals(icase.passive_part) => 1.0F
						case _ => 0.0F
					}
				case _ => 0.0F
			}
			case false =>0.0F
		}
		return similar
	}

}