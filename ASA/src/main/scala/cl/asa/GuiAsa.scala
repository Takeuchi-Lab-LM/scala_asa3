package cl.asa

import cl.asa.yaml._
import cl.asa.init._
import cl.asa.parse._
import cl.asa.result._
import cl.asa.output._
import scala.swing._
import scala.swing.event.ButtonClicked
/**
 * GUIによるASAのmainとなるところ
 */
object GuiAsa extends SimpleSwingApplication {
	//print("起動中\r")
    var analyzer: String = "cabocha -n1 -f1"

	private val file = new YamlFile()
	private val dicts = new LoadYaml(file)
	private val parser = new Parse(dicts,analyzer)
	private val output = new Output("test")
	private val goutput = new GuiOutput()
	private val otype = "all"
    
	def top = new MainFrame {
		title = "ASA"
		minimumSize = new Dimension(300, 200)
		contents = mainPanel
	}
    
	
	val mainPanel = new BoxPanel(Orientation.Vertical) {
		val b1 = new Button("解析")
		val t1 = new TextField("彼は開ける")
		contents += t1
		contents += b1
		b1.reactions += {
			case ButtonClicked(b1) => {
				val result = parser.parse(t1.text)
				goutput.outputAll(result)
				//selectOutput(otype, result)
			}
		}
	}

	private def selectOutput(otype: String, result: Result) {
		otype match {
			case "all" => output.outputAll(result)
			case "semantic" => output.outputSemantic(result)
			case "yaml" => output.outputYaml(result)
		}
	}
}
