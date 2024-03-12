package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day23 (inputData : String) {
  
    val inputList : List[String] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    inputList.foreach(println)

    class Plateau(content : List[String]) {
        val sizeX = content.map(_.size).max
        val sizeY = content.size
        val twile : Array[Array[Int]] = Array.ofDim[Int](sizeX,sizeY)
        for (i <- 0 until sizeX) {
            for (j<- 0 until sizeY) {
                
            }
        }
        

    }


    def resultExo() : Int = {
        val p = new Plateau(inputList)
        return 0
    }


    def resultExoP2() : Int = {
        return 0
    }

}


object Day23 extends App {
    
    val data = "data/day23_input_test.txt"
    val exo = new Day23(data)
    
    println(s"Exercice Day 23 : [${exo.resultExo}]")
    println(s"Exercice Day 23 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day23