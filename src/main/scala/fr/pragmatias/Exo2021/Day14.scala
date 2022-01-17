package fr.pragmatias.Exo2021

import fr.pragmatias.Tools
import scala.annotation.tailrec

class Day14 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)






    class PolymerMap(listPolymer : List[String]) {
        val mapping = listPolymer.map(x => x.split(" -> ")).map(x => (x(0),x(1))).toMap

        def getPolymerElement(pair : String) : String = {
            return mapping.getOrElse(pair,"")
        }

        def getPolymerPairsWithElement(pair : String) : List[String] = {
            val tmp = mapping.getOrElse(pair,"")
            return List(pair.head.toString+tmp,tmp+pair.last.toString)
        }
    }

    /*def insertPolymer(template : String, mapping : PolymerMap) : String = {
        val templateSize = template.length
        return (0 until templateSize-1).map(x => (template.substring(x,x+1),template.substring(x,x+2))).map(x => x._1+mapping.getPolymerElement(x._2)).foldLeft("")(_+_) + template.last
    }*/

    @tailrec
    final def insertPolymer(template : String, target : String, mapping : PolymerMap) : String = {
        if (template.length == 1) {
            return target + template
        } 
            
        return insertPolymer(template.tail ,target + template.head.toString + mapping.getPolymerElement(template.take(2)),mapping)
    }

    def stepInsertionOfPolymer(template : String, step : Int, mapping : PolymerMap) : String = {
        return (1 to step).foldLeft(template)({case (template,stepTmp) => /*println(s"Start step : [${stepTmp}]");*/ insertPolymer(template,"",mapping)})
    }

    def mostCommonElement(template : String) : (Long,String) = {
        return template.groupBy(identity).map(x => (x._2.length.toLong,x._1.toString)).max
    }

    def leastCommonElement(template : String) : (Long,String) = {
        return template.groupBy(identity).map(x => (x._2.length.toLong,x._1.toString)).min
    }


    def resultExo() : Long = {
        //println(inputList.head)
        val polymer = stepInsertionOfPolymer(inputList.head,10,new PolymerMap(inputList.tail)) // can't calcul more than 10 or 15 step
        //println(polymer)
        return mostCommonElement(polymer)._1 - leastCommonElement(polymer)._1
    }



    def resultP2(template : String, step : Int, mapping : PolymerMap) : Long = {

        val CountingPolymer : Map[String,Long] = (0 until template.size-1).map(x => template.substring(x,x+2)).map(x => (x,1L)).groupMapReduce(_._1)(_._2)(_+_)
        //println(CountingPolymer)

        var countPolyumerTmp = (1 to step).foldLeft(CountingPolymer)({case (a,b) => a.map(x => mapping.getPolymerPairsWithElement(x._1).map(y => (y,x._2))).flatten.groupMapReduce(_._1)(x => x._2)(_+_)})
        //println(countPolyumerTmp)

        val res = countPolyumerTmp.toList.map(x => (x._1.head,x._2)).groupMapReduce(_._1)( x => x._2 )( _+_ ).map(x => if (x._1.toString == template.last.toString) { (x._1,x._2+1L) } else {x})
        //println(res)

        val leastCommon = res.minBy(_._2)._2
        val mostCommon = res.maxBy(_._2)._2
        //println(leastCommon)
        //println(mostCommon)

        return mostCommon - leastCommon
    }



    def resultExoP2() : Long = {
        return resultP2(inputList.head,40,new PolymerMap(inputList.tail))
    }

}


object Day14 extends App {
    
    val data = "data/day14_input.txt"
    val exo = new Day14(data) 
    
    println(s"Exercice Day 14 : [${exo.resultExo}]")
    println(s"Exercice Day 14 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day14