package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Bingo (content : List[String], num : Int) {

    var bingoContent : List[List[(Int,Boolean)]] = content.map(x =>  x.trim.split("\\s+")).map(x => x.toList.map(y => (y.toInt,false)))
    val bingoID : Int = this.num
    var winner : Boolean = false

    def newNumber( number : Int) : Unit = {
        bingoContent = bingoContent.map(x => x.map(y => if (y._1 == number) {(y._1,true)} else {y}))
    }

    def hasWin() : Boolean = {
        for (i <- 0 until 5) {
            if (extractColumn(i).filter(_._2).size == 5) { winner = true }
            if (extractLine(i).filter(_._2).size == 5) { winner = true }
        }
        return winner
    }


    def extractColumn(id : Int) : List[(Int,Boolean)] = {
        return bingoContent.map(x => x(id))
    }
    
    def extractLine(id : Int) : List[(Int,Boolean)] = {
        return bingoContent(id).toList
    }

    def sumAllUnmarked () : Int = {
        val bingoContentUnmarked : List[List[Int]] = bingoContent.map(x => x.filter(!_._2).map(_._1))
        return bingoContentUnmarked.map(_.foldLeft(0)({(a,b) => a+b})).foldLeft(0)({(a,b) => a+b})
    }


    override def toString : String = {
        var s = ""
        bingoContent.foreach({ x => 
            x.foreach({ y => 
                s+=s"(${y._1},${y._2})"
            })
            s+="\n"
        })
        return s"Bingo [${bingoID}]\n${s}"
    }


}


class BingoSystem (content : List[String]) {

    val listNumber : Array[Int] = content(0).split(",").map(_.toInt)
    var listBingo : List[Bingo] = List()
    var result : Int = 0

    for (i <- 1 until content.size by 5) {
        listBingo = listBingo ::: List(new Bingo(content.slice(i,i+5),((i-1)/5)+1))
    }


    def playNumber(b : Bingo, num: Int) : Bingo = {
        b.newNumber(num)
        return b
    }

    def GiantSquid() : BingoSystem = {
        
        var end : Boolean = false
        var cpt : Int = 0
        var num : Int = 0
        var resultTmp : List[Bingo] = List()
        while (cpt < listNumber.size && !end) {
            if (!end) {
                //print(s"\n")
                num = listNumber(cpt)
                //print(s"tirage : ${num}")
                listBingo = listBingo.map(x => playNumber(x,listNumber(cpt)))
                resultTmp = listBingo.filter(x => x.hasWin)
                if (resultTmp.size > 0) {
                    //println("\nWinner food !")
                    //resultTmp.foreach({x=>println(x)})
                    end = true
                }
                //listBingo.foreach({x => println(x)})
                cpt+=1
            } 

        }

        val bingoWinner = resultTmp(0)
        val sumUnmarked = bingoWinner.sumAllUnmarked
        result = sumUnmarked * num

        return this
    }


    def GiantSquidP2() : BingoSystem = {
        
        var cpt : Int = 0
        var num : Int = 0
        var resultTmp : List[(Int,List[Bingo])] = List()
        for (cpt <- 0 until listNumber.size) {
            num = listNumber(cpt)
            //println(s"tirage : ${num}")
            listBingo = listBingo.filter(x => !x.hasWin).map(x => playNumber(x,listNumber(cpt)))
            resultTmp = resultTmp:::List((num,listBingo.filter(x => x.hasWin)))

        } 

        val (bingoLastNum,bingoWinnerList) = resultTmp.filter(x => !x._2.isEmpty).last
        val sumUnmarked = bingoWinnerList.last.sumAllUnmarked
        result = sumUnmarked * bingoLastNum

        return this
    }

}


class Day04 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)


    def resultExo() : Int = {
        val bingoSystemGiantSquid = new BingoSystem(inputList).GiantSquid()
        return bingoSystemGiantSquid.result
    }


    def resultExoP2() : Int = {
        val bingoSystemGiantSquidReverse = new BingoSystem(inputList).GiantSquidP2()
        return bingoSystemGiantSquidReverse.result
    }

}



object Day04 extends App {
    
    val data = "data/day04_input.txt"
    val exo = new Day04(data) 
    
    println(s"Exercice Day 04 : [${exo.resultExo}]")
    println(s"Exercice Day 04 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day04