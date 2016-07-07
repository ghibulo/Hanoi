package tutorial.webapp

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
//import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery


object TutorialApp extends JSApp {

/*
def appendPar(targetNode: dom.Node, text: String): Unit = {
  val parNode = document.createElement("p")
  val textNode = document.createTextNode(text)
  parNode.appendChild(textNode)
  targetNode.appendChild(parNode)
}

*/
//@JSExport
def addClickedMessage(): Unit = {
  //appendPar(document.body, "You clicked the button!")
  jQuery("body").append("<p>Kliknul jsi!!</p>")
}

def setupUI(): Unit = {
  jQuery("#click-me-button").click(addClickedMessage _)
  jQuery("body").append("<p>Hello World</p>")
}

  def main(): Unit = {
  //  println("Hello world!")
  jQuery(setupUI _)
  /*appendPar(document.body, "Čau světe!")
  appendPar(document.body, "Jak se máš?")*/
  }
}


