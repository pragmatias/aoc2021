package fr.pragmatias.Tools

import scala.io.Source

class FileInput (filename : String) {

    val listContent = Source.fromFile(this.filename).getLines().toList

    def getList : List[String] = {
        return listContent
    }

    override def toString : String = {
        return listContent.take(10).mkString("\n")
    }
  
}
