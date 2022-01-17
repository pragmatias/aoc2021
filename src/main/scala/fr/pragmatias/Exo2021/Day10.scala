package fr.pragmatias.Exo2021

import fr.pragmatias.Tools
import scala.annotation.tailrec


class Day10 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    def isBegingChar(char : String) : Boolean = {
        return char match {
            case "(" | "[" | "{" | "<" => true
            case _ => false 
        }
    }

    def calculEndingValue (char : String) : Int = {
        return char match {
            case ")" => 3
            case "]" => 57
            case "}" => 1197
            case ">" => 25137
            case _ => 0 
        }
    }

    def calculEndingValueP2 (char : String) : Int = {
        return char match {
            case ")" => 1
            case "]" => 2
            case "}" => 3
            case ">" => 4
            case _ => 0 
        }
    }

    def isEndingExpected(beginChar : String, endingChar : String) : Boolean = {
        return endingChar match {
            case ")" =>  beginChar == "("
            case "]"  =>  beginChar == "["
            case "}" =>  beginChar == "{"
            case ">" =>  beginChar == "<"
            case _ => false 
        }
    }

    def replaceByExpectedEnding(beginChar : String) : String = {
        return beginChar match {
            case "(" => ")"
            case "[" => "]"
            case "{" => "}"
            case "<" => ">"
            case _ => "" 
        }
    }

    def getFirstCorruptedChar(ligne : String) : (List[String],List[Int],Int) = {
        return getFirstCorruptedChar(ligne.split("").toList,List(0),1)
    }

    @tailrec
    final def getFirstCorruptedChar(listChar : List[String], beginCharAt : List[Int], endingCharAt : Int) : (List[String],List[Int],Int) = {
        if (endingCharAt >= listChar.size) {
            return (listChar,beginCharAt,-1)
        } else if (beginCharAt.isEmpty && (!isBegingChar(listChar(endingCharAt)))) {
            return (listChar,beginCharAt,-1) 
        } 

        val beginCharAtLast = if (beginCharAt.size > 0) { beginCharAt.last } else {-1}
        val beginChar = if (beginCharAtLast != -1) { listChar(beginCharAtLast) } else {""}
        val endingChar = listChar(endingCharAt)

        if (beginCharAtLast == -1) {
            return getFirstCorruptedChar(listChar,List(endingCharAt),endingCharAt+1)
        } else if (isBegingChar(endingChar)) {
            return getFirstCorruptedChar(listChar,(beginCharAt ::: List(endingCharAt)),endingCharAt+1)
        } else {
            if (isEndingExpected(beginChar,endingChar)) {
                return getFirstCorruptedChar(listChar,beginCharAt.dropRight(1),endingCharAt+1)
            } else {
                return (listChar,beginCharAt,endingCharAt)
            }
        }
    }


    def completeCorruptedLine(listChar : List[String], beginCharAt : List[Int]) : Long = {
        return beginCharAt.map(x => calculEndingValueP2(replaceByExpectedEnding(listChar(x)))).reverse.foldLeft(0L)((a,b) => (5L*a) + b.toLong)
    }

    def mediane(list : List[Long]) : Long = {
        return list(((list.size.toDouble / 2).round.toInt)-1)
    }



    def resultExo() : Int = {
        return inputList.map(getFirstCorruptedChar(_)).filter(x => x._3 > 0).map(x => calculEndingValue(x._1(x._3))).sum
    }


    def resultExoP2() : Long = {
        val t = inputList.map(getFirstCorruptedChar(_)).filter(x => x._3 == -1).map(x => completeCorruptedLine(x._1,x._2)).sorted
        return mediane(t)
    }

}


object Day10 extends App {
    
    val data = "data/day10_input.txt"
    val exo = new Day10(data) 
    
    println(s"Exercice Day 10 : [${exo.resultExo}]")
    println(s"Exercice Day 10 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day10