package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day09 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    class Plateau(content : List[String]) {
        val lignes = inputList.size
        val colonnes = inputList(0).length
        var plateau = Array.ofDim[(Int,Int)](lignes,colonnes) //(heigh,adjacentHighPoint)
        var bassins = Array.ofDim[(Int,Int)](lignes,colonnes) //(heigh,bassinID)
        for (i <- 0 until lignes) {
            for ( j <- 0 until colonnes) {
                plateau(i)(j) = (content(i)(j).toString.toInt,0)
                bassins(i)(j) = (content(i)(j).toString.toInt,0)
            }
        }
        

        def majLowPoints() : Plateau = {
            for (i <- 0 until lignes) {
                for ( j <- 0 until colonnes) {
                    plateau(i)(j) = (plateau(i)(j)._1,getHighPointsAdjacent(i,j))
                }
            }
            return this
        }

        def getHighPointsAdjacent(ligne:Int,colonne:Int) : Int = {
            val listAdjacent = List((1,0),(-1,0),(0,1),(0,-1)).map(x => (x._1+ligne,x._2+colonne)).filter(x => x._1 >= 0 && x._1 < lignes && x._2 >= 0 && x._2 < colonnes)
            return listAdjacent.map(x => plateau(x._1)(x._2)._1).filter(x => plateau(ligne)(colonne)._1 >= x).size
        }

        def simuleBassin(ligne:Int,colonne:Int,idBassin:Int) : Int = {
            val currentElement = bassins(ligne)(colonne)
            val listAdjacent = List((1,0),(-1,0),(0,1),(0,-1)).map(x => (x._1+ligne,x._2+colonne)).filter(x => currentElement._2 == 0 && currentElement._1 < 9).filter(x => x._1 >= 0 && x._1 < lignes && x._2 >= 0 && x._2 < colonnes)
            val elementBassinsAdjacent = listAdjacent.map(x => ((x),bassins(x._1)(x._2))).filter({case (x,y) => (currentElement._1 <= y._1) || (currentElement._1 >= y._1) })
            val listBassinID = elementBassinsAdjacent.map({case (coord,eltB) => eltB._2}).filter(_ > 0)
            var bassinID = idBassin
            if (listBassinID.size > 0) {
                val bassinIDTmp = listBassinID.max
                if (bassinIDTmp != bassinID && bassinIDTmp > 0) {
                    bassinID = bassinIDTmp
                }
            } 
            // maj bassins
            if (elementBassinsAdjacent.size > 0) {
                bassins(ligne)(colonne) = (currentElement._1,bassinID)
            }
            // maj adjacent bassins
            elementBassinsAdjacent.foreach({ case (coord,_) => simuleBassin(coord._1,coord._2,bassinID)})
            return elementBassinsAdjacent.size
        }

        def majBassins() : Plateau = {
            var numBassin = 1
            for (i <- 0 until lignes) {
                for ( j <- 0 until colonnes) {
                    val nbrBassinSimule = simuleBassin(i,j,numBassin)
                    if (nbrBassinSimule != 0) { numBassin += 1 }
                }
            }
            return this
        }

        def countBassins() : Long = {
            return bassins.flatMap(x => x.filter(_._2 > 0)).groupBy(_._2).map(x => x._2.size.toLong).toSeq.sortWith(_ > _ ).take(3).foldLeft(1L)(_*_)
        }

        def countLowPoints() : Int = {
            return plateau.flatMap(x => x.filter(_._2 == 0).map( _._1 + 1)).sum
        }

        def affichePlateau() : Unit =  {
            for (i <- 0 until lignes) {
                for ( j<- 0 until colonnes) {
                    print(plateau(i)(j))
                }
                println()
            }
        }

       def afficheBassins() : Unit =  {
            for (i <- 0 until lignes) {
                for ( j<- 0 until colonnes) {
                    print(bassins(i)(j))
                }
                println()
            }
        }

    }


    def resultExo() : Int = {
        val plat = new Plateau(inputList).majLowPoints()
        //plat.affichePlateau
        return plat.countLowPoints()
    }


    def resultExoP2() : Long = {
        val plat = new Plateau(inputList).majBassins()
        //plat.afficheBassins
        return plat.countBassins
    }

}


object Day09 extends App {
    
    val data = "data/day09_input.txt"
    val exo = new Day09(data) 
    
    println(s"Exercice Day 09 : [${exo.resultExo}]")
    println(s"Exercice Day 09 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day09