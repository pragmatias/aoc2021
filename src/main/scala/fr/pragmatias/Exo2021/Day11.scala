package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day11 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)


    class Octopus(energy : Int) { 
        var energyLevel : Int = energy
        var mustFlash : Boolean = false
        var hasFlashed : Boolean = false

        def addEnergy(level : Int) : Unit = {
            energyLevel += level
            if (energyLevel == 10) { mustFlash = true }
        }

        def hasToFash : Boolean = {
            return mustFlash
        }

        def flashedOctopus : Unit = {
            hasFlashed = true
            mustFlash = false
            energyLevel = 0
        }

        def hasAlreadyFlashed : Boolean = {
            return hasFlashed
        }

        def cleanFlash : Unit = {
            mustFlash = false
            hasFlashed = false
        }

        override def toString : String = {
            return s"{${energyLevel}}"
        }


    }

    class Cavern (content : List[String]) {
        val limitLines : Int = content.size
        val limitColumns : Int = content(0).length
        var space = Array.ofDim[Octopus](limitLines,limitColumns)
        for (i <- 0 until limitLines) {
            for ( j <- 0 until limitColumns) {
                space(i)(j) = new Octopus(content(i)(j).toString.toInt)
            }
        }

        def octopusFlashAdjacent(line : Int, column : Int) : Unit = {
            if (space(line)(column).hasToFash) {
                List((1,0),(0,1),(-1,0),(0,-1),(1,1),(-1,-1),(1,-1),(-1,1)).map(x => (x._1+line,x._2+column))
                            .filter(x => x._1 >= 0 && x._2 >= 0 && x._1 < limitLines && x._2 < limitColumns)
                            .foreach({x => if (!space(x._1)(x._2).mustFlash && !space(x._1)(x._2).hasFlashed ) {space(x._1)(x._2).addEnergy(1)}})
                space(line)(column).flashedOctopus
            }
        }


        def octopusLife() : Cavern = {
            //step1 : 1 more energy
            space.foreach(x => x.foreach(y => y.addEnergy(1)))
 
            //step2 : flashing octopus
            var endWhile = false
            while (!endWhile) {
                for (i <- 0 until limitLines) {
                    for ( j <- 0 until limitColumns) {
                        octopusFlashAdjacent(i,j)
                    }
                }
                endWhile = space.map(x => x.filter(y => y.hasToFash).size).sum == 0
            }

            // step3 : Reset flash
            space.foreach(x => x.foreach(y => y.cleanFlash))

            return this
        }

        def countFlashedOctopus() : Long = {
            return space.map(x => x.filter(y => y.energyLevel == 0).size).sum
        }

        def afficheSpace() : Unit = {
            for (i <- 0 until limitLines) {
                for ( j <- 0 until limitColumns) {
                    print(space(i)(j))
                }
                println
            }
        }

    }


    def countTotalFlashedOctopus(numberOfStep : Int, cavern : Cavern) : Long = {
        return (1 to numberOfStep).foldLeft((cavern,0L))( (a,b) => { val cav = a._1.octopusLife() ; (cav,cav.countFlashedOctopus()+a._2)})._2
    }

    def firstStepWhereAllFlashedSynchro(cavern : Cavern) : Long = {
        val cavernSize = cavern.limitColumns * cavern.limitLines
        var endWhile = false
        var step = 0L

        while (!endWhile) {
            cavern.octopusLife()
            step+=1L
            if (cavern.countFlashedOctopus() == cavernSize || step > 1000L) {
                endWhile = true
            }
        }
        return step
    }


    def resultExo() : Long = {
        val cavern = new Cavern(inputList)
       // (0 to 1).foreach(x => {t.octopusLife(); t.afficheSpace ;println()})
        return countTotalFlashedOctopus(100,cavern)
    }


    def resultExoP2() : Long = {
        val cavern = new Cavern(inputList)
        return firstStepWhereAllFlashedSynchro(cavern)
    }

}


object Day11 extends App {
    
    val data = "data/day11_input.txt"
    val exo = new Day11(data) 
    
    println(s"Exercice Day 11 : [${exo.resultExo}]")
    println(s"Exercice Day 11 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day11