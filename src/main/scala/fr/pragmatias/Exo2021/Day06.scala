package fr.pragmatias.Exo2021

import fr.pragmatias.Tools

class Day06 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)
    val populationInit : Map[Int,Long] = (0 to 8).groupMapReduce(identity)(_ => 0L)(_+_) ++ inputList(0).trim.split(",").toList.map(_.toInt).groupMapReduce(identity)(_ => 1L)(_+_)

    def nextLife(population : Map[Int,Long]) : Map[Int,Long] = {
        return population.keys.map(x => (x ,( x match { 
                                                        case 0 => population.getOrElse(1,0L)
                                                        case 1 => population.getOrElse(2,0L)
                                                        case 2 => population.getOrElse(3,0L)
                                                        case 3 => population.getOrElse(4,0L)
                                                        case 4 => population.getOrElse(5,0L)
                                                        case 5 => population.getOrElse(6,0L)
                                                        case 6 => population.getOrElse(7,0L) + population.getOrElse(0,0L)
                                                        case 7 => population.getOrElse(8,0L)
                                                        case 8 => population.getOrElse(0,0L)
                                                        }
                                            )
                                        )
                ).toMap
    } 


    def simulate(nbrJour : Int) : Long = {
        val endSimulation = (1 to nbrJour).foldLeft(populationInit)({case (a,b) => nextLife(a)})
        return endSimulation.values.sum
    }


    def resultExo() : Long = {
        return simulate(80)
    }

    def resultExoP2() : Long = {
        return simulate(256)
    }

}

object Day06 extends App {
    
    val data = "data/day06_input.txt"
    val exo = new Day06(data) 

    println(s"Exercice Day 06 : [${exo.resultExo}]")
    println(s"Exercice Day 06 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day06