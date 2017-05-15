package cl.asa

import cl.asa.yaml._
import cl.asa.init._
import cl.asa.parse._
import cl.asa.result._
import cl.asa.output._
/**
 *   Copyright (c) 2015 Takeuchi Laboratory
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * ASAのmainとなるところ
 * @todo 今のところ引数はファイル解析の際のファイル名のみになっているが，出力タイプの指定やYAMLファイルの指定も行いたい
 */

object Asa {
	private var parser: cl.asa.parse.Parse = null
	private val output = new Output("test")
	private var file = ""
	def main(args: Array[String]): Unit = {
		print("起動中\r")
		val start = System.currentTimeMillis()
		parser = this.startUp
		val end = System.currentTimeMillis()
		val time: Float = end - start
		val used: Float = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / 1024
		println("%.3f".format(used / 1024) + "MB: " + "%.3f".format(time / 1000) + "秒")
		var infile: String = ""
		var itype: String = "all"

		args.foreach { arg =>
			arg match {
				case "-x" => itype = "xml"
				case _ => infile = arg
			}
		}
		infile.isEmpty match {
			case true => this.inputConsole(itype)
			case false => this.inputFile(infile, itype)
		}
		parser.end
		println("exit")
	}

	/*
	 * 起動の時に引数にファイル名があればファイルの内容を解析する
	 */

	def startUp: Parse = {
		val files = new YamlFile()
		val dicts = new LoadYaml(files)
		val parser = new Parse(dicts)
		return parser
	}

	def startUpCabocha: CabochaParse = {
		val parser = new CabochaParse
		return parser
	}

	private def inputFile(file: String, itype: String) {
		val sorce = scala.io.Source.fromFile(file)
		sorce.getLines.foreach { line =>
			val result = parser.parse(line)
			this.selectOutput(itype, result)
		}
	}

	/*
	 * 起動の時に引数になにもなければコンソールに入力されたものを解析する
	 */
	private def inputConsole(itype: String) {
		var input: String = ""
		while ({ println("input"); input = readLine; !input.equals("") & !input.equals(" ") }) {
			val result = parser.parse(input)
			this.selectOutput(itype, result)
		}
	}

	/*
	 * 出力の様式を選択
	 * all:　		解析により付与されたすべての要素を出力
	 * semantic:　	語義，意味役割関係のみ出力
	 * yaml:　		解析データをyamlファイルとして出力
	 */
	private def selectOutput(otype: String, result: Result) {
		otype match {
			case "all" => output.outputAll(result)
			case "xml" => output.outputXML(result)
			case "semantic" => output.outputSemantic(result)
			case "yaml" => output.outputYaml(result)
		}
	}
}
