package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day08 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    class SchemaClassic() {
        val schema = Map( 0 -> "abcef"
                        , 1 -> "cf"
                        , 2 -> "acdeg"
                        , 3 -> "acdfg"
                        , 4 -> "bcdf"
                        , 5 -> "abdfg"
                        , 6 -> "abdefg"
                        , 7 -> "acf"
                        , 8 -> "abcdefg"
                        , 9 -> "abcdfg")
        
        val schemaList = schema.map(x => (x._1,x._2.split("").toList)).toMap

        val schemaInverse = schema.map(x => (x._2.sorted,x._1)).toMap

        def getList(size : Int) : List[String] = {
            return size match {
                case 2 => schemaList.getOrElse(1,List())
                case 3 => schemaList.getOrElse(7,List())
                case 4 => schemaList.getOrElse(4,List())
                case 5 => (schemaList.getOrElse(2,List()) ::: schemaList.getOrElse(3,List()) ::: schemaList.getOrElse(5,List()))
                case 6 => schemaList.getOrElse(0,List()) ::: schemaList.getOrElse(6,List()) ::: schemaList.getOrElse(9,List())
                case 7 => schemaList.getOrElse(8,List())
                case _ => List()
            }
        }
    }

    class SchemaWired(listNumber : String, schemaClassic : SchemaClassic) {
        var schemaWired = Map ( "a" -> "" 
                            ,"b" -> ""
                            ,"c" -> ""
                            ,"d" -> ""
                            ,"e" -> ""
                            ,"f" -> ""
                            ,"g" -> "")

        var schemaSeeking : Map[String,List[String]] = Map()

        val content = listNumber.trim.split("\\s+").groupBy(_.size).map(x => (x._1,x._2.toList.foldLeft("")(_+_)))
                                .toSeq.sortBy(_._1)
        
        val schema = schemaClassic


        def letterPossible (letterSource: String, letterTarget: String, schema : Map[String,List[String]]) : Boolean = {
            return (!schema.filter(x => x._1 == letterSource && x._2.contains(letterTarget)).isEmpty
                    || schema.filter(x => x._1 != letterSource && x._2.contains(letterTarget)).isEmpty)
        }
        def cleanSchemaSeeking (schematoClean : Map[String,List[String]]) : Map[String,List[String]] = {
            val letterFound = schematoClean.filter(x => x._2.size == 1).map(x => (x._1,x._2(0)))
            var schemaSeekingTmp = schematoClean
            letterFound.foreach({ case (x,y) => {
                schemaSeekingTmp = schemaSeekingTmp.map({ case(z1,z2) => {
                    if (z1 != x) { (z1,z2.filter(_ != y)) } else { (z1,z2) }
                }})
            }})
            return schemaSeekingTmp
        }


        def addNumber(numberSize : Int, numberContent : String, schemaFound : Map[String,List[String]]) : Map[String,List[String]] = {
            // get the number content (group by letter + sum of occurence)
            val listWorkableTmp = numberContent.split("").groupBy(identity).map(x => (x._1,x._2.size)).toMap
            // get the schemaClassic content (group by letter + sum of occurence) based on the number
            val listSchema = schema.getList(numberSize).groupBy(identity).map(x => (x._1,x._2.size)).toMap
            val listWorkable = listWorkableTmp.map({case (a,b) => (a,listSchema.filter(_._2 == b).map(_._1))}).toMap

            var schemaSeekingTmp = schemaFound
            listWorkable.foreach({case (x,y)  => {
                val listeLetterPossible = y.filter(letter => letterPossible(x,letter,schemaFound)).toList
                schemaSeekingTmp = schemaSeekingTmp ++ Map(x -> listeLetterPossible) 
                }
            })
            //println(schemaSeeking.toSeq.sortBy( _._1))
            return cleanSchemaSeeking(schemaSeekingTmp)
        }

        def translateWiredtoNumber(number : String) : Int = {
            return schema.schemaInverse.getOrElse(number.split("").map(x => schemaWired.getOrElse(x,"")).sorted.foldLeft("")(_+_),0)
        }

        def resolve() : SchemaWired = {
            val mapInit : Map[String,List[String]] = Map()
            val mapResolved = content.foldLeft(mapInit)({case(a,b) => addNumber(b._1,b._2,a)})
            //schemaWired = mapResolved.map({case(x,y) => if (y.size == 1) {(x,y(0))} else {(x,"")}})
            schemaWired = mapResolved.map({case(x,y) => (x,y.head)})
            //println("Schema : "+schemaWired)
            return this
        }


    }

    class SystemWired (listNumber : String) {

        val schemaClassic = new SchemaClassic()

        val schemaRes = new SchemaWired(listNumber,schemaClassic).resolve()
        
        def translateWiredtoNumber (outputNumbers : String) : String = {
            return outputNumbers.split("\\s+").map(x => schemaRes.translateWiredtoNumber(x.trim).toString).foldLeft("")(_+_)
        }
                   
    }

    def matchingWired() : Int = {
       
        return 0
    }

    def resultExo() : Int = {
        val outputValues = inputList.map(_.split("\\|")(1).trim)
                                    .flatMap(_.split("\\s+")
                                    .map(_.trim))
                                    .map(_.length)
                                    .filter(_ match {
                                                    case 2 | 3 | 4 | 7 => true
                                                    case _ => false})
                                    .size
        return outputValues
    }


    def resultExoP2() : Int = {
        val outputValues = inputList.map(x => (x.split("\\|")(0),x.split("\\|")(1))).map(x => (new SystemWired(x._1.trim)).translateWiredtoNumber(x._2.trim))
        
        //println(outputValues)
        //val t = outputValues.map(new SystemWired(_))

        return outputValues.map(_.toInt).sum
    }

}


object Day08 extends App {
    
    val data = "data/day08_input.txt"
    val exo = new Day08(data) 
    
    println(s"Exercice Day 08 : [${exo.resultExo}]")
    println(s"Exercice Day 08 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day08