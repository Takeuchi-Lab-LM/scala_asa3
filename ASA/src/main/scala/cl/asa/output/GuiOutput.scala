package cl.asa.output
import cl.asa.result._
import scala.swing._
import scala.swing.event.ButtonClicked

class GuiOutput {
	def outputAll(result: Result) {
		val frame = new Frame
		val tabPane = new TabbedPane
		frame.title = result.surface
		frame.minimumSize = new Dimension(300, 200)
		tabPane.minimumSize = new Dimension(200, 200)
		tabPane.background = new Color(255, 255, 255)
		for (chunk <- result.chunks) {
			tabPane.pages += addTab(chunk)
		}
		frame.contents =tabPane
		frame.open
	}

	def bigLabel(str:String):Label ={
		
		val label = new Label(str)
		label.font = new Font("Dialog",0,30)
		return label
	}
	
	def addTab(chunk: Chunk): TabbedPane.Page = {
		
		val pane = new BoxPanel(Orientation.Vertical) {
			
			background = new Color(255, 255, 255)
			contents += bigLabel(chunk.surface)
			contents += bigLabel("\tlink: " + chunk.link)
			contents += bigLabel("\ttype: " + chunk.ctype)
			if (!chunk.main.isEmpty()) { contents += bigLabel("\tmain: " + chunk.main) }
			if (!chunk.part.isEmpty) { contents += bigLabel("\tpart: " + chunk.part) }
			if (!chunk.category.isEmpty) { contents += bigLabel("\tcategory: " + chunk.category.mkString("")) }
			if (chunk.semrole.nonEmpty) { contents += bigLabel("\tsemrole: " + chunk.semrole.mkString("|")) }
			if (chunk.score > 0F) { contents += bigLabel("\tscore: " + chunk.score) }
			if (!chunk.semantic.isEmpty()) { contents += bigLabel("\tsemantic: " + chunk.semantic) }
			if (!chunk.modifiedchunks.isEmpty) {
				var str = "\tframe: "
				for (modchunk <- chunk.modifiedchunks) {
					if (modchunk.semrole.nonEmpty) { str = str + modchunk.id + "," + modchunk.semrole.mkString("|") + " " }
				}
				contents += bigLabel(str)
			}
			if (chunk.idiom.nonEmpty) {
				contents += bigLabel("\tidiom: " + chunk.idiom)
				contents += bigLabel("\tfilter: " + chunk.idiom_score)
			}
			if (chunk.idiom_morph.nonEmpty) {
				val ids = chunk.idiom_morph.map(morph => morph.chunk.id + "-" + morph.id)
				contents += bigLabel("\tidiom_id: " + ids.mkString(","))
			}
			if (chunk.phrase.nonEmpty) { contents += bigLabel("\tphrase: " + chunk.phrase.mkString(",")) }
			if (!chunk.voice.isEmpty()) { contents += bigLabel("\tvoice: " + chunk.voice) }
			if (!chunk.tense.isEmpty()) { contents += bigLabel("\ttense: " + chunk.tense) }
			if (!chunk.sentelem.isEmpty()) { contents += bigLabel("\tsentelem: " + chunk.sentelem) }
			if (!chunk.polarity.isEmpty()) { contents += bigLabel("\tpolarity: " + chunk.polarity) }
			if (!chunk.mood.isEmpty()) { contents += bigLabel("\tmood: " + chunk.mood) }
		}
		val page = new TabbedPane.Page(chunk.id.toString, pane)
		return page
	}

}