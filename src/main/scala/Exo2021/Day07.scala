package fr.pragmatias.Exo2021

import fr.pragmatias.Tools
import scala.annotation.tailrec


class Day07 (inputData : String) {
  
    val inputList : List[String] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)
    val crabList : List[Int] = inputList(0).split(",").toList.map(_.toInt)


    def mediane (data : List[Int]) : Int = {
        val dataSorted : List[Int] = data.sorted
        val cpt : Int = dataSorted.size
        val even : Boolean = (cpt % 2 == 0)
        
        if (even) {
            return dataSorted((cpt/2)-1)
        } else {
            return (dataSorted((cpt/2)-1) + dataSorted((cpt/2)))/2
        }
    }

    def factoriel (n : Int) : Int = {
        return n match {
            case 0 => 0
            case _ => n + factoriel (n-1)
        }
    }

    case class Crab(pos : Int, fuel : Int)

    @tailrec
    final def simulate(data : List[Crab]) : Int = {
        //println(data)
        if (data.forall(x => x.pos == data.head.pos)) { 
          //  println(data.map(x => factoriel(x.fuel)))
            return data.map(x => factoriel(x.fuel)).sum 
        } else {
            val limitMin = data.minBy(_.pos).pos
            val limitMax = data.maxBy(_.pos).pos

            val moveMaxfuelcost = data.filter(_.pos == limitMax).map(x => x.fuel+1).sum
            val moveMinfuelcost = data.filter(_.pos == limitMin).map(x => x.fuel+1).sum
            //println(s"costmin = [${moveMinfuelcost}] - costmax [${moveMaxfuelcost}]")

            return (
                    if (moveMinfuelcost > moveMaxfuelcost) {
                        simulate(data.map({ x => 
                                            if (x.pos == limitMax) {
                                                new Crab(x.pos-1,x.fuel+1)
                                            } else { x }
                                        }))
                    } else {
                        simulate(data.map({ x => 
                                            if (x.pos == limitMin) {
                                                new Crab(x.pos+1,x.fuel+1)
                                            } else { x }
                                        }))
                    }
            )

        }
    }



    def resultExo() : Int = {
        val med = mediane(crabList)
        val listing = crabList.map(x => { if(x > med) {x - med} else {med - x} })
        val fuel = listing.sum
        return fuel
    }


    def resultExoP2() : Int = {
        return simulate(crabList.map(x => new Crab(x,0)))
    }

}


object Day07 extends App {
    
    val data = "data/day07_input.txt"
    val exo = new Day07(data) 
    
    println(s"Exercice Day 07 : [${exo.resultExo}]")
    println(s"Exercice Day 07 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day07