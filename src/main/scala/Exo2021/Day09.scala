package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day09 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    class Plateau(content : List[String]) {
        val lignes = inputList.size
        val colonnes = inputList(0).length
        var tableau = Array.ofDim[(Int,Int)](lignes,colonnes)
        for (i <- 0 until lignes) {
            for ( j <- 0 until colonnes) {
                tableau(i)(j) = (content(i)(j).toString.toInt,0)
            }
        }

        def majLowPoints() : Plateau = {
            for (i <- 0 until lignes) {
                for ( j <- 0 until colonnes) {
                    val up = (if ((i-1) >= 0) { tableau(i-1)(j)._1 } else {10})
                    val down = (if ((i+1) < lignes) { tableau(i+1)(j)._1 } else {10})
                    val left = (if ((j-1) >= 0) { tableau(i)(j-1)._1 } else {10})
                    val right = (if ((j+1) < colonnes) { tableau(i)(j+1)._1 } else {10})
                    val current = tableau(i)(j)._1
                    var cpt = 0
                    if (current > up ) { cpt+=1 }
                    if (current > down ) { cpt+=1 }
                    if (current > left ) { cpt+=1 }
                    if (current > right ) { cpt+=1 }
                    tableau(i)(j) = (current,cpt)
                }
            }
            return this
        }


        def countLowPoints() : Int = {
            return tableau.flatMap(x => x.filter(_._2 == 0).map( _._1 + 1)).sum
        }

        def affichePlateau() : Unit =  {
            for (i <- 0 until lignes) {
                for ( j<- 0 until colonnes) {
                    print(tableau(i)(j))
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


    def resultExoP2() : Int = {
        return 0
    }

}


object Day09 extends App {
    
    val data = "data/day09_input_test.txt"
    val exo = new Day09(data) 
    
    println(s"Exercice Day 09 : [${exo.resultExo}]")
    println(s"Exercice Day 09 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day09