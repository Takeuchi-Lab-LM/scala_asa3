import scala.io.Source
import scala.math._
import cl.asa.yaml._
import cl.asa.init._
import cl.asa.parse._
import cl.asa.result._
import cl.asa.yaml.frame._
import fileIO._
import scala.collection.JavaConversions._

/**
 * 正解データとASAの解析結果の比較
 */
object compare {
	//パラメータ
	val infile: String = ""
    val analyzer: String = "cabocha -n1 -f1"

	print("起動中\r")
	private val Files = new YamlFile()
	private val Dicts = new LoadYaml(Files)
	private val Parser = new Parse(Dicts,analyzer)
	private val Reader = new CsvReader()
	private var Scount = Map("all" -> 0, "true" -> 0, "false" -> 0)
	private var Rcount = Map("all" -> 0, "true" -> 0, "false" -> 0)
	private var SemanticResult = Map("true" -> 0F, "precision" -> 0F, "recall" -> 0F)
	private var RoleResult = Map("true" -> 0F, "precision" -> 0F, "recall" -> 0F, "precision2" -> 0F, "recall2" -> 0F)

	def main(args: Array[String]): Unit = {
		println("-start-")
		val csvs = Source.fromFile(infile).getLines.map(Reader.parseLine(_).get).toSeq
		csvs.tail.foreach { csv =>
			println(csv(14))
			val asa = Parser.parse(csv(14))
			compareResult(asa, csv)
		}
		this.OutputResult
		println("-done-")
	}

	/**
	 * 結果の比較
	 */
	def compareResult(asa: Result, csv: List[String]) {
		val roles = csv.slice(1, 13).grouped(2).filter(_(1).nonEmpty).toSeq

		Scount = Scount.updated("all", Scount("all") + 1)
		SemanticResult = SemanticResult.updated("recall", SemanticResult("recall") + 1)
		Rcount = Rcount.updated("all", Rcount("all") + roles.size)
		RoleResult = RoleResult.updated("recall", RoleResult("recall") + roles.size)
		
		val verb = csv(0) //見出し語
		val semantic = csv(15) //概念フレーム
		val candicate_chunks = asa.chunks.filter { chunk => //対象となる述語のchunkの候補を取得(1つの文に同じ述語が複数存在する場合のため)
			chunk.main.equals(verb) //|| chunk.surface.equals(verb)
		}
		val chunk = candicate_chunks.applyOrElse(csv(13).toInt, (_: Int) => null) //対象となる述語のchunk取得

		chunk.ne(null) match {
			case true =>
				SemanticResult = SemanticResult.updated("precision", SemanticResult("precision") + 1)
				RoleResult = RoleResult.updated("precision", RoleResult("precision") + chunk.modifiedchunks.count(mod => mod.semrole.nonEmpty))
				this.compareSemantic(chunk, semantic)
				this.compareRole(chunk, roles)
			case false =>
		}
	}

	/**
	 * 語義の比較
	 */
	def compareSemantic(chunk: Chunk, semantic: String) {
		semantic.equals(chunk.semantic) match {
			case true =>
				Scount = Scount.updated("true", Scount("true") + 1)
				SemanticResult = SemanticResult.updated("true", SemanticResult("true") + 1)
			case false =>
				Scount = Scount.updated("false", Scount("false") + 1)
		}
	}

	/**
	 * 意味役割の比較
	 */
	def compareRole(chunk: Chunk, roles: Seq[List[String]]) {
		val pairs = this.getMatchChunk(chunk, roles)
		pairs.foreach { pair =>
			val (modchunk, roles) = pair
			val role = roles(0).split("（")(0) //意味役割
			val term = roles(1) //
			modchunk.semrole.map(_.split("（")(0)).contains(role) match {
				case true =>
					Rcount = Rcount.updated("true", Rcount("true") + 1)
					RoleResult = RoleResult.updated("true", RoleResult("true") + 1)
				case false =>
					Rcount = Rcount.updated("false", Rcount("false") + 1)
			}

			//bccwj、CaboChaによるミスを除いたpresison recallのカウント
			RoleResult = RoleResult.updated("recall2", RoleResult("recall2") + 1)
			modchunk.semrole.nonEmpty match {
				case true => RoleResult = RoleResult.updated("precision2", RoleResult("precision2") + 1)
				case false =>
			}
		}
	}

	/**
	 * ASAの解析と正解データでchunkが一致するペアを取得
	 */
	def getMatchChunk(chunk: Chunk, roles: Seq[List[String]]): Seq[(Chunk, List[String])] = {
		val pairs = roles.map { role =>
			chunk.modifiedchunks.reverse.find(mod => role(1).contains(mod.surface)) match {
				case Some(modchunk) => (modchunk, role)
				case None => null
			}
		}.filter(_.ne(null))
		return pairs
	}

	def OutputResult {
		println("全語義: " + Scount("all"))
		println(" 語義の一致: " + Scount("true"))
		println(" 語義の不一致: " + Scount("false"))
		println(" 取れなかった動詞: " + (Scount("all") - Scount("true") - Scount("false")))
		println("precision\t" + (SemanticResult("true") / SemanticResult("precision")) * 100 + "%")
		println("recall\t" + (SemanticResult("true") / SemanticResult("recall")) * 100 + "%")

		println
		println("全意味役割: " + Rcount("all"))
		println(" 意味役割の一致: " + Rcount("true"))
		println(" 意味役割の不一致: " + Rcount("false"))
		println(" 取れなかったchunk: " + (Rcount("all") - Rcount("true") - Rcount("false")))
		println("presicion\t" + (RoleResult("true") / RoleResult("precision")) * 100 + "%")
		println("recall\t" + (RoleResult("true") / RoleResult("recall")) * 100 + "%")
		println("presicion2\t" + (RoleResult("true") / RoleResult("precision2")) * 100 + "%")
		println("recall2\t" + (RoleResult("true") / RoleResult("recall2")) * 100 + "%")
	}

	def checkAsa(asa: Chunk, vth: List[String]) {
		println(vth(29) + ":" + asa.main)
		val semantic = vth.slice(30, 35).mkString("-")
		println("-ASA-")
		println(asa.semantic)
		for (mod <- asa.modifiedchunks) {
			println(mod.main + mod.part + ": " + mod.semrole + ": " + mod.score)
		}
		println("-vth-")
		println(semantic)
		val frames = Dicts.frames.getFrame(asa.main).get.frame
		val sem = frames.find(_.semantic.equals(semantic)).get
		val inst = sem.instance.find(_.cases.exists(t => vth(29).contains(t.noun))).get
		for (t <- inst.cases) {
			println(t.noun + t.part + ": " + t.semrole + ": " + t.weight)
		}

	}
}
