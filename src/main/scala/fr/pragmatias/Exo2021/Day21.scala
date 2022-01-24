package fr.pragmatias.Exo2021

import fr.pragmatias.Tools

import scala.annotation.tailrec


class Day21 (inputData : String) {

    val inputList : List[String] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    def getPlayers : List[Player] = inputList.map({
        case s"Player $n starting position: $p" => new Player(n.toInt, p.toInt,0)
        case _ => new Player(0, 0,0)
    })

    val dice : Dice = new Dice(100)

    class Dice(val limit : Int) {
        var lastValue : Int = 0
        var rollsNumber : Int = 0

        def rolls( nb : Int ) : Int = {
            var sumValue : Int = 0
            for (n <- 1 to nb) {
                val resTmp = ((lastValue+n)%limit)
                val tmp = (if (resTmp == 0) (limit) else resTmp)
                //println(tmp)
                sumValue += tmp
            }
            lastValue=(if (lastValue+nb > limit) ((lastValue+nb)%limit) else (lastValue+nb))
            rollsNumber+=nb
            //println(sumValue)
            return sumValue
        }
        override def toString : String = s"Dice Rolls [${rollsNumber}] : lastValue [${lastValue}] - Limit [${limit}]"
    }

    class Player(val number : Int, var position : Int, var score : Int) {
        var winUniverse : Long = 0

        def move(moveValue : Int): Unit = {
            val resTmp : Int = (position+moveValue)%10
            position = (if (resTmp == 0) (10) else (resTmp))
            score += position
        }

        def moveWithUniverse(moveValue : Int) : Player = {
            val playerTmp = new Player(this.number,this.position,this.score)
            playerTmp.move(moveValue)
            return playerTmp
        }

        override def toString : String = s"Player [${number}] : Pos [${position}] - Score [${score}] - winUniverse [${winUniverse}]"
    }


    @tailrec
    final def executeGame(score : Int, nbRolls : Int, dice : Dice, players : List[Player]) : (List[Player],Dice) = {
        if (players.exists(_.score >= score)) {
            return (players,dice)
        } else {
            players.head.move(dice.rolls(nbRolls))
            //println(players.head)
            //println(dice)
            executeGame(score,nbRolls,dice,players.tail:::List(players.head))
        }
    }


    val diceQuantumCombi : List[(Int,Int)] = List(1,2,3).flatMap(x => List(1,2,3).map(y => x+y)).flatMap(x => List(1,2,3).map(y => x+y)).groupMapReduce(identity)(_ => 1)(_+_).toList

    def executeGameQuantum(score : Int, players : (Player,Player), nbrUniverse : Long) : (Long,Long) = {
        if (players._2.score >= score) {
            //println(s"${players._2.number} --> Win [${nbrUniverse}]")
            if (players._2.number == 1) {
                return (nbrUniverse,0L)
            } else {
                return (0L,nbrUniverse)
            }
        } else {
            var res = (0L,0L)
            for (diceQ <- diceQuantumCombi) {
                val newNbrUniverse = nbrUniverse * diceQ._2
                val resTmp = executeGameQuantum(score,(players._2,players._1.moveWithUniverse(diceQ._1)),newNbrUniverse)
                res = (res._1+resTmp._1 , res._2+resTmp._2)
            }
           // println(res)
            return res
        }
    }




    def resultExo() : Int = {
        val (listPlayers,diceResult) = executeGame(1000,3,dice,getPlayers)
        val looser = listPlayers.filter(_.score < 1000).maxBy(_.score)
        //println(looser)
        //println(dice)
        return (looser.score * dice.rollsNumber)
    }


    def resultExoP2() : Long = {
        val players = getPlayers
        val playersResult = executeGameQuantum(21,(players(0),players(1)),1L)
        return List(playersResult._1,playersResult._2).max
    }

}


object Day21 extends App {

    val data = "data/day21_input.txt"
    val exo = new Day21(data)

    println(s"Exercice Day 21 : [${exo.resultExo}]")
    println(s"Exercice Day 21 Part2 : [${exo.resultExoP2}]")

}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day21