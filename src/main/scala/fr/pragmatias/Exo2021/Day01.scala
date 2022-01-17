package fr.pragmatias.Exo2021

import fr.pragmatias.Tools

class Day01 (inputData : String) {
  
    val inputList : List[Int] = new Tools.FileInput(this.inputData).listContent.map(_.toInt)


    def resultExo : Int = {
        var previousValue = inputList(0)
        var currentValue = 0
        var resultTmp = 0
        var listTmp : List[(Int,Int)] = List((previousValue,resultTmp))
        for (i <- 1 until inputList.size) {
            var currentValue = inputList(i)
            if (currentValue > previousValue) {
                resultTmp = 1
            } else {
                resultTmp = 0
            }
            listTmp = listTmp++List((currentValue,resultTmp))
            previousValue = currentValue
        }

        return listTmp.filter(_._2 == 1).size
    } 



    def resultExoLight : Int = {
        var res = 0
        for (i <- 1 until inputList.size) {
            if (inputList(i) > inputList(i-1)) {
                res+=1
            }
        }
        return res
    }


    def resultExoZip : Int = {
        return inputList.zip(inputList.tail).count({ case (prev, aft) => aft > prev})
    } 



    def resultExoP2 : Int = {
        var res = 0
        var listNewInput : List[Int] = List()
        for (i <- 0 until inputList.size-2) {
            var current = inputList(i) + inputList(i+1) + inputList(i+2)
            listNewInput = listNewInput ::: List(current)
        }

        for (i <- 1 until listNewInput.size) {
            if (listNewInput(i) > listNewInput(i-1)) {
                res+=1
            }
        }
        
        return res
    }
    
    
    
    def resultExoP2Zip : Int = {
        var res = 0
        var listNewInput : List[Int] = List()
        val inputListTriplet = inputList.zip(inputList.tail).zip(inputList.tail.tail).map({case((a,b),c) => a+b+c})
        return inputListTriplet.zip(inputListTriplet.tail).count({case (a,b) => b > a})
    }

}


object Day01 extends App {
    
    val data_day01 = "data/day01_input.txt"
    val exo_day01 = new Day01(data_day01)
    
    println(s"Exercice Day 01 first: [${exo_day01.resultExo}]")
    println(s"Exercice Day 01 zip: [${exo_day01.resultExoZip}]")
    println(s"Exercice Day 01 light : [${exo_day01.resultExoLight}]")
    println(s"Exercice Day 01 part2 : [${exo_day01.resultExoP2}]")
    println(s"Exercice Day 01 part2 Zip : [${exo_day01.resultExoP2Zip}]")
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day01