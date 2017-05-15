package cl.asa.parse.cabocha
/**
 * Cabochaを起動し，解析結果を取得するクラス
 */
import scala.io._
import cl.asa.result._
import java.io._
import scala.sys.process._

class Cabocha(path: String, code: String) {
	private val cabocha = Process(path)
	private val process = Runtime.getRuntime().exec(path)
	private val is = process.getInputStream();
	private val br = new BufferedReader(new InputStreamReader(is, code)) //出力
	private val os = process.getOutputStream();
	private val pw = new PrintWriter(new OutputStreamWriter(os, code), true) //入力

	def parse(input: String): Result = {
		var m_id: Int = 0
		val result: Result = new Result(input)
		pw.println(input) //かぼちゃに入力
		var line: String = ""
		while (!line.equals("EOS")) { //nullだと終了しなかった
			line = br.readLine()
			if (line.startsWith("* ")) {
				result.addChunk(new Chunk(line))
				m_id = 0
			} else if (!line.equals("EOS")) {
				result.chunks.last.addMorph(new Morph(m_id, line))
				m_id = m_id + 1
			}
		}
		return result
	}
	def end() = {
		is.close(); os.close(); process.getErrorStream().close()
		process.destroy()
	}
}
