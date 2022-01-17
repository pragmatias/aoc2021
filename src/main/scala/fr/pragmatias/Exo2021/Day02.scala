package fr.pragmatias.Exo2021

import fr.pragmatias.Tools

class Day02 (inputData : String) {
  
    val inputList : List[(String,Int)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty).map(_.split(" ")).map(x => (x(0).toLowerCase,x(1).toInt))

    def resultExo() : Int = {
        var deep : Int = 0
        var hori : Int = 0
        for (i <- 0 to inputList.size-1) {
            var move = inputList(i)._1
            var num =  inputList(i)._2
            move match {
                case "up" => deep-=num
                case "down" => deep+=num
                case "forward" => hori+=num
                case _ => println("Error")
            }
        }

        return deep*hori
    }



    def resultExoP2() : Int = {
        var deep : Int = 0
        var hori : Int = 0
        var aim : Int = 0
        for (i <- 0 to inputList.size-1) {
            var move = inputList(i)._1
            var num =  inputList(i)._2
            move match {
                case "up" => {aim-=num}
                case "down" => {aim+= num}
                case "forward" => {hori+=num; deep+=(aim*num)}
                case _ => println("Error")
            }
        }

        return deep*hori
    }

}



object Day02 extends App {
    
    val data = "data/day02_input.txt"
    val exo = new Day02(data) 
    
    println(s"Exercice Day 02 : [${exo.resultExo}]")
    println(s"Exercice Day 02 Part2 : [${exo.resultExoP2}]")
    
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day02