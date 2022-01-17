package fr.pragmatias.Exo2021

import fr.pragmatias.Tools
import scala.annotation.tailrec


class Day17 (inputData : String) {
  
    val inputList : List[String] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

  
    case class Coord (x : Int, y : Int)
    case class Velocity (vx : Int, vy : Int)

    class Objectif (content : String) {
        val inputX : String = content.split(",")(0).split("=")(1)
        val inputY : String = content.split(",")(1).split("=")(1)
        val minX : Int = inputX.split("\\.\\.")(0).toInt
        val maxX : Int = inputX.split("\\.\\.")(1).toInt
        val minY : Int = inputY.split("\\.\\.")(0).toInt
        val maxY : Int = inputY.split("\\.\\.")(1).toInt

        def isInObjectif(probe : Coord) : Boolean = {
            return (minX <= probe.x && maxX >= probe.x && minY <= probe.y && maxY >= probe.y)
        }

        def isTooFar(probe : Coord) : Boolean = {
            return probe.x > maxX || probe.y < minY
        }
    }

    def mvtProbes(probe : Coord , velo : Velocity) : (Coord,Velocity) = {
        val veloX = if (velo.vx > 0) { -1 }  else if (velo.vx == 0) { 0 } else { 1 }
        return (Coord(probe.x+velo.vx,probe.y+velo.vy),Velocity(velo.vx+veloX,velo.vy-1))
    }

    def simuleProbesVelocity(velo : Velocity, obj : Objectif) : (Int,Boolean) = {
        var probe = Coord(0,0)
        return simuleProbesVelocityRec(probe,0,velo,obj)
    }

    @tailrec
    final def simuleProbesVelocityRec(probe : Coord, y : Int, velo : Velocity, obj : Objectif) : (Int,Boolean) = {
        if (obj.isInObjectif(probe)) {
            return (y,true)
        } else if(obj.isTooFar(probe)) {
            return (0,false)
        }
        val tmp = mvtProbes(probe,velo)
        val yMax = if (tmp._1.y > y) tmp._1.y else y
        return simuleProbesVelocityRec(tmp._1,yMax,tmp._2,obj)
    }



    def searchVelocityMaxY(obj : Objectif) : (Velocity,Int) = {
        val perimetre = (1 to obj.maxX).flatMap(x => (1 to (obj.maxX.abs + obj.maxY.abs)).map(y => Velocity(x,y)))
        
        val test = perimetre.map(x => (x,simuleProbesVelocity(x,obj))).filter(x => x._2._2).sortBy(x => - x._2._1).head
        return (test._1,test._2._1)
    }

    def searchNumberVelocityDistinctP2(obj : Objectif) : Int = {
        val perimetre = (1 to obj.maxX).flatMap(x => (obj.minY to obj.minY.abs).map(y => Velocity(x,y)))
        val t = perimetre.map(x => (x,simuleProbesVelocity(x,obj)._2)).filter(x => x._2).map(x => (x._1.vx,x._1.vy)).distinct
        //println(t)
        return t.size
    }

    def resultExo() : Int = {
        val obj = new Objectif(inputList(0))
        val velo = searchVelocityMaxY(obj)
        return velo._2
    }


    def resultExoP2() : Int = {
        val obj = new Objectif(inputList(0))
        return searchNumberVelocityDistinctP2(obj)
    }

}


object Day17 extends App {
    
    val data = "data/day17_input.txt"
    val exo = new Day17(data) 
    
    println(s"Exercice Day 17 : [${exo.resultExo}]")
    println(s"Exercice Day 17 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day17