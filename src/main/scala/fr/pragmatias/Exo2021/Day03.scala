package fr.pragmatias.Exo2021

import fr.pragmatias.Tools

class Day03 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    def resultExo() : Int = {
        val sizeEnter : Int = inputList(0).length
        var resultTmp : List[(Int,Int)] = List()
        for (i <- 0 to sizeEnter-1) {
            resultTmp = resultTmp:::List(inputList.map({x => if (x(i).toString.toInt == 0) {(1,0)} else {(0,1)}}).foldLeft((0,0))({(x,y)=>(x._1+y._1,x._2+y._2)}))
        }

        var gamma = ""
        var epsilon = ""

        for (i  <- 0 to sizeEnter-1 ) {
            val contentResultTmp = resultTmp(i)
            gamma += {if (contentResultTmp._1 > contentResultTmp._2) { "0" } else { "1" } }
            epsilon += {if (contentResultTmp._1 < contentResultTmp._2) { "0" } else { "1" } }
        }

        val gammaInt = Integer.parseInt(gamma,2)
        val epsilonInt = Integer.parseInt(epsilon,2)

        return  gammaInt * epsilonInt 
    }


    def getMostOrLeastCommonValues(listData : List[String], pos : Int) : (Int,Int) = {
        return listData.map({x => if (x.substring(pos,pos+1) == "0") {(1,0)} else {(0,1)}}).foldLeft((0,0))({(x,y)=>(x._1+y._1,x._2+y._2)})
    }

    def defineLeastCommonValues(values : (Int,Int)) : String = {
        return if (values._1 > values._2) { "1" } 
        else if (values._1 < values._2) { "0" }
        else { "0" }
    }

    def defineMostCommonValues(values : (Int,Int)) : String = {
        return if (values._1 > values._2) { "0" } 
        else if (values._1 < values._2) { "1" }
        else { "1" }
    }

    def resultExoP2() : Int = {
        val sizeEnter : Int = inputList(0).length
        var inputListFilterOxygen = inputList
        var inputListFilterCO2 = inputList



        var resultTmp : List[(Int,Int)] = List()
        for (i <- 0 to sizeEnter-1) {
            if (inputListFilterOxygen.size > 1) {
                val valuesOxygen = getMostOrLeastCommonValues(inputListFilterOxygen,i)
                inputListFilterOxygen = inputListFilterOxygen.filter(x => x.substring(i,i+1) == defineMostCommonValues(valuesOxygen))
            }

            if (inputListFilterCO2.size > 1) {
                val valuesCO2 = getMostOrLeastCommonValues(inputListFilterCO2,i)
                inputListFilterCO2 = inputListFilterCO2.filter(x => x.substring(i,i+1) == defineLeastCommonValues(valuesCO2))
            }
        }

        // oxygen rating (most common)
        // CO2 rating (least common)
        val oxygenResult = inputListFilterOxygen(0)
        val co2Result = inputListFilterCO2(0)
        
        //println(oxygenResult)
        //println(co2Result)

        return Integer.parseInt(oxygenResult,2) * Integer.parseInt(co2Result,2)
    }

}



object Day03 extends App {
    
    val data = "data/day03_input.txt"
    val exo = new Day03(data) 
    
    println(s"Exercice Day 03 : [${exo.resultExo}]")
    println(s"Exercice Day 03 Part2 : [${exo.resultExoP2}]")
    
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day03