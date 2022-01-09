package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day12 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)


    class Grotte(grotte: String) {
        val isStart = grotte == "start"
        val isEnd = grotte == "end"
        val isSmall = grotte == grotte.toLowerCase
        val isBig = grotte == grotte.toUpperCase
        val name = this.grotte

        override def toString : String = {
            return grotte
        }
    }

    class Link(grottes : List[Grotte]) {
        def getExitPossible(enter : String) : List[Grotte] = {
            return grottes.filter(x => x.name != enter)
        }

        def isLinkContain(name : String) : Boolean = {
            return (grottes.filter(x => x.name == name ).size > 0)
        }
    }


    class MapCaves(links : List[String]) {
        val cave = links.map(_.split("-")).map( x => new Link(List(new Grotte(x(0)),new Grotte(x(1)))))

        def getExitPossible(entrance : Grotte) : List[Grotte] = {
            return cave.filter(x => x.isLinkContain(entrance.name)).flatMap(w => w.getExitPossible(entrance.name)).distinct
        }

        def checkPathConstraint(parcour : List[Grotte]) : Boolean = {
            return parcour.head.isStart && (parcour.filter(x => x.isSmall).groupBy(_.name).filter(x => x._2.size > 1).size == 0)
        }

        def checkPathConstraintP2(parcour : List[Grotte]) : Boolean = {
            return ( parcour.head.isStart 
                    && (parcour.filter(x => x.isSmall && !x.isStart && !x.isEnd).groupBy(_.name).filter(x => x._2.size > 1).size <= 1)
                    && (parcour.filter(x => x.isSmall && !x.isStart && !x.isEnd).groupBy(_.name).filter(x => x._2.size > 2).size == 0)
                    && (parcour.filter(x => x.isStart || x.isEnd).groupBy(_.name).filter(x => x._2.size > 1).size == 0) )
        }

        def checkPathEnding(parcour : List[Grotte]) : Boolean = {
            return parcour.last.isEnd
        }

        def listAllPath(fConstraint: List[Grotte] => Boolean) : List[List[Grotte]] = {
            var allPaths : List[List[Grotte]] = List(List())
            return listPaths(fConstraint,new Grotte("start"),allPaths)
        }


        def listPaths(fConstraint: List[Grotte] => Boolean, step : Grotte, paths : List[List[Grotte]]) : List[List[Grotte]] = {
            val listNextStep = getExitPossible(step)
            var listPathtoEnd : List[List[Grotte]] = List(List())
            var listPathToContinue : List[List[Grotte]] = List(List())
            //println(s"Step : ${step}")

            if (paths.isEmpty) { 
                listPathToContinue = List(List(step))
            } else {
                listPathToContinue = paths.map( x => x:::List(step)).filter(x => fConstraint(x) && !checkPathEnding(x))
                listPathtoEnd  = paths.map( x => x:::List(step)).filter(x => fConstraint(x) && checkPathEnding(x))
            }
            if (listPathToContinue.isEmpty) {
                return listPathtoEnd
            }          

            val listPathTmp = listNextStep.flatMap(x => listPaths(fConstraint,x,listPathToContinue))

            return listPathtoEnd ::: listPathTmp
        }


    }



    def resultExo() : Int = {
        val cave = new MapCaves(inputList)
        //val t = cave.listAllPath()
        //t.foreach(println(_))
        return cave.listAllPath(cave.checkPathConstraint).size
    }


    def resultExoP2() : Int = {
        val cave = new MapCaves(inputList)
        //val t = cave.listAllPath()
        //t.foreach(println(_))
        return cave.listAllPath(cave.checkPathConstraintP2).size
    }

}


object Day12 extends App {
    
    val data = "data/day12_input.txt"
    val exo = new Day12(data) 
    
    println(s"Exercice Day 12 : [${exo.resultExo}]")
    println(s"Exercice Day 12 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day12