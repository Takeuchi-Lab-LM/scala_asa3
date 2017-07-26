package cl.asa.output
/**
 * 解析結果を出力するためのクラス
 * @todo ファイルに結果を出力するようにも選択できるようにしたい
 */
import cl.asa.result._
import scala.xml._
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

class Output(file: String) {

	/**
	 * 語義と意味役割のみを出力
	 * @param　ASAの解析結果
	 */
	def outputSemantic(result: Result) {
		println(result.surface)
		println
		for (chunk <- result.chunks) {
			if (!chunk.ctype.equals("elem")) {
				for (link <- chunk.modifiedchunks) {
					print(link.surface + "\t")
				}
				print(chunk.main)
				if (!chunk.tense.isEmpty()) {
					print("+ed")
				}
				println("\t" + chunk.ctype)
				for (link <- chunk.modifiedchunks) {
					link.semrole.nonEmpty match {
						case true => print(link.semrole.mkString(",") + "\t")
						case false => print(link.adjunct + "\t")
					}
				}
				println(chunk.semantic)
				println
			}
		}
	}

	/**
	 * ASAで付与するデータをすべて出力
	 * @param　ASAの解析結果
	 */

	def outputXML(result: Result) {
		val xml = <sent>
			<sentence>{ result.surface }</sentence>
			{ result.chunks.map(this.outputChunkXML(_)) }
		</sent>
		val format = new scala.xml.PrettyPrinter(80, 2).format(xml)
		println(format)
	}

	private def outputChunkXML(chunk: Chunk): xml.Elem = {
		var elems: Seq[xml.Elem] = Seq.empty
		elems :+= <surface>{ chunk.surface }</surface>
		elems :+= <link>{ chunk.link }</link>
		elems :+= <type>{ chunk.ctype }</type>
		elems :+= <noun_surface>{ chunk.main }</noun_surface>
		if (chunk.category.nonEmpty) elems :+= <category>{ chunk.category }</category>
		if (chunk.category.nonEmpty) elems :+= <part_surface> { chunk.part } </part_surface>
		if (chunk.semantic.nonEmpty) elems :+= <semantic>{ chunk.semantic }</semantic>
		if (chunk.semrole.nonEmpty) elems :+= <semrole>{ chunk.semrole.mkString("|") }</semrole>
		if (chunk.similar > 0) elems :+= <similar>{ chunk.similar }</similar>
		if (chunk.voice.nonEmpty) elems :+= <voice>{ chunk.voice }</voice>
		if (chunk.mood.nonEmpty) elems :+= <mood>{ chunk.mood }</mood>
		if (chunk.polarity.nonEmpty) elems :+= <polarity>{ chunk.polarity }</polarity>
		if (chunk.sentelem.nonEmpty) elems :+= <sentelem>{ chunk.sentelem }</sentelem>
		if (chunk.modifiedchunks.nonEmpty) elems ++= chunk.modifiedchunks.map(mod => <mod>{ mod.id }</mod>)
		if (chunk.idiom.nonEmpty) {
			elems :+= <idiom>
				<entry>{ chunk.idiom }</entry>
				<filter>{ chunk.idiom_score }</filter>
				{ chunk.idiom_morph.map(morph => <morph>{ morph.chunk.id }-{ morph.id }</morph>) }
				<prase>{ chunk.phrase }</prase>
			</idiom>
		}
		val chunkxml = <chunk id={ chunk.id.toString }>
			{ elems }
			{ chunk.morphs.map(this.outputMorphXML(_)) }
		</chunk>
		return chunkxml
	}

	private def outputMorphXML(morph: Morph): xml.Elem = {
		var elems: Seq[xml.Elem] = Seq.empty
		elems :+= <surface>{ morph.surface }</surface>
		elems :+= <read>{ morph.read }</read>
		elems :+= <base>{ morph.base }</base>
		elems :+= <pos>{ morph.pos }</pos>
		elems :+= <ctype>{ morph.ctype }</ctype>
		elems :+= <cform>{ morph.cform }</cform>
		elems :+= <ne>{ morph.ne }</ne>
		if (morph.forms.ne(null)) {
			elems :+= <cchart>
				<type>{ morph.ctype }</type>
				{ morph.forms.toArray.map(form => <form>{ form }</form>) }
			</cchart>
		}
		val morphxml = <morph id={ morph.id.toString }>{ elems }</morph>
		return morphxml
	}

	def outputAll(result: Result) {
		println("sentence: " + result.surface)
		result.chunks.foreach { chunk =>
			this.outputChunk(chunk)
		}
	}

	private def outputChunk(chunk: Chunk) {
		println("ID: " + chunk.id + " " + chunk.surface)
		println("\tlink: " + chunk.link)
		println("\ttype: " + chunk.ctype)
		if (chunk.main.nonEmpty) { println("\tmain: " + chunk.main) }
		if (chunk.part.nonEmpty) { println("\tpart: " + chunk.part) }
		if (chunk.category.nonEmpty) { println("\tcategory: " + chunk.category.applyOrElse(0, "")) }
		if (chunk.semrole.nonEmpty) { println("\tsemrole: " + chunk.semrole.mkString("|")) }
    if (chunk.arg.nonEmpty) { println("\targ: " + chunk.arg.mkString("|")) }
		if (chunk.similar > 0F) { println("\tscore: " + chunk.similar) }
		if (chunk.semantic.nonEmpty) { println("\tsemantic: " + chunk.semantic) }
		//val modchunks = this.getModChunk(chunk)
    val modchunks = chunk.modifiedchunks
		if (modchunks.nonEmpty) {
			val frame = modchunks.collect {
				case modchunk if modchunk.semrole.nonEmpty => modchunk.id + "-" + modchunk.semrole.mkString("|") + "-" + modchunk.arg.mkString("|")
				case modchunk => modchunk.id + "-" + modchunk.ctype
			}
			println("\tframe: " + frame.mkString(","))
		}
		if (chunk.idiom.nonEmpty) {
			println("\tidiom: " + chunk.idiom)
			println("\tfilter: " + chunk.idiom_score)
			val ids = chunk.idiom_morph.map(morph => morph.chunk.id + "-" + morph.id)
			println("\tidiom_id: " + ids.mkString(","))
		}
		if (chunk.phrase.nonEmpty) { println("\tphrase: " + chunk.phrase.mkString(",")) }
		if (chunk.voice.nonEmpty) { println("\tvoice: " + chunk.voice) }
		if (chunk.tense.nonEmpty) { println("\ttense: " + chunk.tense) }
		if (chunk.sentelem.nonEmpty) { println("\tsentelem: " + chunk.sentelem) }
		if (chunk.polarity.nonEmpty) { println("\tpolarity: " + chunk.polarity) }
		if (chunk.mood.nonEmpty) { println("\tmood: " + chunk.mood) }
		chunk.morphs.foreach { morph =>
			this.outputMorph(morph)
		}
		if (chunk.noun_agentiveL.nonEmpty) { println("\tnoun_adjective: " + chunk.noun_agentiveL) }
		if (chunk.noun_arg.nonEmpty) { println("\tnoun_arg: " + chunk.noun_arg) }
		if (chunk.noun_semantic.nonEmpty) { println("\tnoun_semantic: " + chunk.noun_semantic) }
		if (chunk.noun_semrole.nonEmpty) { println("\tnoun_semrole: " + chunk.noun_semrole) }
		if (chunk.noun_semantic.nonEmpty) {
			val frame = modchunks.collect {
				case modchunk if modchunk.noun_arg.nonEmpty & modchunk.noun_agentiveRole.nonEmpty =>
					modchunk.id + "-" + modchunk.noun_arg + "-" + modchunk.noun_agentiveRole
				case modchunk if modchunk.noun_arg.nonEmpty =>
					modchunk.id + "-" + modchunk.noun_arg
			}
			println("\tnoun_agentiveRole: " + frame.mkString(","))
		}
	}

	private def outputMorph(morph: Morph) {
		val morphs: Seq[String] = Seq(morph.id.toString, morph.surface, morph.read, morph.base, morph.pos, morph.cform, morph.ctype, morph.ne)
		println(morphs.mkString("\t\t", "\t", ""))
	}

	/**
	 * YAML形式で出力
	 * @param　ASAの解析結果
	 *
	 * @todo まだ出力していない要素あり(idiom関係とか)
	 */
	def outputYaml(result: Result) {
		println("---")
		println("result: ")
		println(" " * 2 + "sentence: " + result.surface)
		outputYamlChunk(result.chunks)

	}

	private def outputYamlChunk(chunks: Seq[Chunk]) {
		println(" " * 2 + "chunks: ")
		for (chunk <- chunks) {
			println(" " * 2 + "- chunk: ")
			println(" " * 6 + "id:" + chunk.id)
			println(" " * 6 + "surface: " + chunk.surface)
			println(" " * 6 + "link: " + chunk.link)
			println(" " * 6 + "head: " + chunk.head)
			println(" " * 6 + "fanc: " + chunk.fanc)
			println(" " * 6 + "score: " + chunk.score)
			if (!chunk.modifiedchunks.isEmpty) {
				println(" " * 6 + "modified: ")
				for (mchunk <- chunk.modifiedchunks) {
					println(" " * 6 + "- id: " + mchunk.id)
				}
			}
			println(" " * 6 + "type: " + chunk.ctype)
			println(" " * 6 + "main: " + chunk.main)
			if (!chunk.part.isEmpty) { println(" " * 6 + "part: " + chunk.part) }
			if (!chunk.tense.isEmpty) { println(" " * 6 + "tense: " + chunk.tense) }
			if (!chunk.voice.isEmpty) { println(" " * 6 + "voice: " + chunk.voice) }
			if (!chunk.polarity.isEmpty) { println(" " * 6 + "polartity: " + chunk.polarity) }
			if (!chunk.sentelem.isEmpty) { println(" " * 6 + "sentelem: " + chunk.sentelem) }
			if (!chunk.mood.isEmpty) { println(" " * 6 + "mood: " + chunk.mood) }

			if (!chunk.semantic.isEmpty) { println(" " * 6 + "semantic: " + chunk.semantic) }
			if (!chunk.modifiedchunks.isEmpty) {
				println(" " * 6 + "frames: ")
				for (mchunk <- chunk.modifiedchunks) {
					println(" " * 6 + "- id: " + mchunk.id)
					println(" " * 8 + "semrole: " + mchunk.semrole.mkString("|"))
				}
			}
			if (!chunk.semrole.isEmpty) { println(" " * 6 + "semrole: " + chunk.semrole.mkString("|")) }
			if (!chunk.adjunct.isEmpty) { println(" " * 6 + "adjunct" + chunk.adjunct) }
			if (!chunk.category.isEmpty) { println(" " * 6 + "category: " + chunk.category) }

			outputYamlMorph(chunk.morphs)
		}
	}

	private def outputYamlMorph(morphs: Seq[Morph]) {
		println(" " * 6 + "morphs: ")
		for (morph <- morphs) {
			println(" " * 6 + "- morph: ")
			println(" " * 10 + "id: " + morph.id)
			println(" " * 10 + "surface: " + morph.surface)
			println(" " * 10 + "pos: " + morph.pos)
			println(" " * 10 + "cform: " + morph.cform)
			println(" " * 10 + "ctype: " + morph.ctype)
			println(" " * 10 + "base: " + morph.base)
			println(" " * 10 + "read: " + morph.read)
			println(" " * 10 + "ne: " + morph.ne)
			if (!morph.forms.eq(null)) {
				println(" " * 10 + "forms: ")
				morph.forms.foreach { form =>
					println(" " * 10 + "- form: " + form)
				}
			}
		}
	}
}
