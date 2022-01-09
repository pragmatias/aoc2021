package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day13 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)


    class Paper (content : List[String]) {
        val listCoord = content.map(_.split(",")).map(x => (x(0).toInt,x(1).toInt))
        var limitX = listCoord.map(_._1).max+1
        var limitY = listCoord.map(_._2).max+1
        var page : Array[Array[String]] = Array.ofDim[String](limitY,limitX)

        for (x <- 0 until limitX) {
            for (y <- 0 until limitY) {
                page(y)(x) = "."
            }
        }
        listCoord.foreach(x => page(x._2)(x._1) = "#")


        def useInstruction(instruction : (Int,Int), foldingPaper : Array[Array[String]]) : Array[Array[String]] = {
            val foldX : Boolean = (instruction._1 != 0)
            val foldY : Boolean = (instruction._2 != 0)
            var resLimitX : Int = foldingPaper(0).size
            var resLimitY : Int = foldingPaper.size
            var resPage : Array[Array[String]] = null

            //println("instruction"+instruction)

            if (foldX) {
                // decoupe X
                resLimitX =  instruction._1
                resPage = Array.ofDim[String](resLimitY,resLimitX)
                for ( x <- 0 until resLimitX) {
                    for ( y <- 0 until resLimitY ) {
                        val tmpFold = foldingPaper(y)(resLimitX - x + resLimitX)
                        if (tmpFold == "#") {
                            resPage(y)(x) = tmpFold
                        } else {
                            resPage(y)(x) = foldingPaper(y)(x)
                        }
                    }
                }

            } else {
                // decoupe Y
                resLimitY = instruction._2

                resPage = Array.ofDim[String](resLimitY,resLimitX)
                for ( x <- 0 until resLimitX) {
                    for ( y <- 0 until resLimitY ) {
                        val tmpFold = foldingPaper(resLimitY - y + resLimitY)(x)
                        if (tmpFold == "#") {
                            resPage(y)(x) = tmpFold
                        } else {
                            resPage(y)(x) = foldingPaper(y)(x)
                        }
                    }
                }
            }
            
            return resPage
        }

        def countDotsVisible : Int = {
            return page.map(x => x.filter(_ == "#").size).sum
        }

        def foldPaper(instructionsList : List[String]) : Paper = {
            val instructions = instructionsList.map(x => x.replace("fold along ","").split("=")).map(x => if (x(0) == "x") { (x(1).toInt,0) } else { (0,x(1).toInt)})
            page = instructions.foldLeft(page)({case (a,b) => useInstruction(b,a)})
            limitX = page(0).size
            limitY = page.size
            //println;affichePaper
            return this
        }


        def affichePaper : Unit = {
            for (y <- 0 until limitY)  {
                for (x <- 0 until limitX) {
                    print(page(y)(x))
                }
                println
            }
        }

    }

    def resultExo() : Int = {
        val page = new Paper(inputList.filter(x => !x.startsWith("fold")))
        return page.foldPaper(inputList.filter(x => x.startsWith("fold")).take(1)).countDotsVisible
    }


    def resultExoP2() : Int = {
        val page = new Paper(inputList.filter(x => !x.startsWith("fold")))
        val t = page.foldPaper(inputList.filter(x => x.startsWith("fold")))
        t.affichePaper
        return 0
    }

} 


object Day13 extends App {
    
    val data = "data/day13_input.txt"
    val exo = new Day13(data) 
    
    println(s"Exercice Day 13 : [${exo.resultExo}]")
    println(s"Exercice Day 13 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day13