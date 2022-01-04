package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class DayXX (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)


    def resultExo() : Int = {
        return 0
    }


    def resultExoP2() : Int = {
        return 0
    }

}


object DayXX extends App {
    
    val data = "data/dayXX_input.txt"
    val exo = new DayXX(data) 
    
    println(s"Exercice Day XX : [${exo.resultExo}]")
    println(s"Exercice Day XX Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.DayXX