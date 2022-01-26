package fr.pragmatias.Exo2021

import fr.pragmatias.Tools

class Day22 (inputData : String) {
  
    val inputList : List[String] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    class Dimension(val min: Int, val max : Int) {

        def isInPerimetre(limit : (Int,Int)) : Boolean = (limit._1 == 0 && limit._2 == 0) || (this.min >= limit._1 && this.max <= limit._2)
        def size : Long = (this.max - this.min).toLong + 1L
        override def toString : String = s"(${min},${max})"

        def intersectExist(d:Dimension) : Boolean = this.max >= d.min && this.min <= d.max
        def intersect(d: Dimension) : Dimension = new Dimension(List(d.min,this.min).max,List(d.max,this.max).min)

    }

    
    class Cuboid(val x : Dimension, val y : Dimension, val z : Dimension) {

        def isInPerimetre(limit : (Int,Int)) : Boolean = this.x.isInPerimetre(limit) && this.y.isInPerimetre(limit) && this.z.isInPerimetre(limit)
        def size : Long = this.x.size * this.y.size * this.z.size

        def intersectExist(c: Cuboid) : Boolean = this.x.intersectExist(c.x) && this.y.intersectExist(c.y) && this.z.intersectExist(c.z)
        def intersect(c : Cuboid) : Cuboid = new Cuboid(this.x.intersect(c.x),this.y.intersect(c.y),this.z.intersect(c.z))


        def substract(c : Cuboid) : List[Cuboid] = {
            var listRes : List[Cuboid] = List()
            if (c.x.min != this.x.min) {
                val xDim = new Dimension(List(c.x.min,this.x.min).min,List(c.x.min,this.x.min).max-1)
                listRes ::= new Cuboid(xDim,this.y,this.z)
            } 
            if (c.x.max != this.x.max) {
                val xDim = new Dimension(List(c.x.max,this.x.max).min+1,List(c.x.max,this.x.max).max)
                listRes ::= new Cuboid(xDim,this.y,this.z)
            } 
            if (c.y.min != this.y.min)  {
                val yDim = new Dimension(List(c.y.min,this.y.min).min,List(c.y.min,this.y.min).max-1)
                listRes ::= new Cuboid(c.x,yDim,this.z)
            } 
            if (c.y.max != this.y.max)  {
                val yDim = new Dimension(List(c.y.max,this.y.max).min+1,List(c.y.max,this.y.max).max)
                listRes ::= new Cuboid(c.x,yDim,this.z)
            } 
            if (c.z.min != this.z.min)  {
                val zDim = new Dimension(List(c.z.min,this.z.min).min,List(c.z.min,this.z.min).max-1)
                listRes ::= new Cuboid(c.x,c.y,zDim)
            } 
            if (c.z.max != this.z.max)  {
                val zDim = new Dimension(List(c.z.max,this.z.max).min+1,List(c.z.max,this.z.max).max)
                listRes ::= new Cuboid(c.x,c.y,zDim)
            } 
            return listRes
        }

        override def toString : String = s"x${x},y${y},z${z}"
    }

    def genereCuboidfromInput(content : String) : (String,Cuboid) = {
        val lineTmp = content.split(" ")
        val action = lineTmp(0)
        val coordTmp = lineTmp(1).split(",")
        val xCoord = coordTmp(0).split("=")(1).split("\\.\\.").map(_.toInt)
        val yCoord = coordTmp(1).split("=")(1).split("\\.\\.").map(_.toInt)
        val zCoord = coordTmp(2).split("=")(1).split("\\.\\.").map(_.toInt)
        return (action,new Cuboid(new Dimension(xCoord(0),xCoord(1)),new Dimension(yCoord(0),yCoord(1)),new Dimension(zCoord(0),zCoord(1))))
    }


    def functionOn(listNewCube : List[Cuboid], listCube : List[Cuboid]) : List[Cuboid] = {
        if (listNewCube.isEmpty) {
            return listCube
        } else {
            val newCube = listNewCube.head
            val listTmp : List[Cuboid] = listCube.filter(c => c.intersectExist(newCube) )
            if (listTmp.isEmpty) {
                return functionOn(listNewCube.tail,newCube::listCube)
            } else {
                val newHole = listTmp.head.intersect(newCube)
                val listPartsCube = newCube.substract(newHole)
                return functionOn(listPartsCube:::listNewCube.tail,listCube)
            }
        }
    }

    
    def functionOff(delCube : Cuboid, listCube : List[Cuboid]) : List[Cuboid] = {
        var res : List[Cuboid] = List()
        for (c <- listCube) {
            if (c.intersectExist(delCube)) {
                res = res ::: c.substract(c.intersect(delCube))
            } else {
                res = c::res
            }
        }
        return res        
    }


    def functionReboot(action : String, newCube : Cuboid, listCube : List[Cuboid]) : List[Cuboid] = if (action == "on") (functionOn(List(newCube),listCube)) else (functionOff(newCube,listCube))

    def simulateReboot(content : List[String], limit : (Int,Int)) : List[Cuboid] = {
        var listCuboids : List[Cuboid] = List()
        for ( line <-  content ) {
            //println(line)
            val (action, cube) = genereCuboidfromInput(line)
            if (cube.isInPerimetre(limit)) {
                //println(s"action : ${action} - cube : ${cube}")
                listCuboids = functionReboot(action,cube,listCuboids)
                //listCuboids.foreach(println(_))
                //println(listCuboids.foldLeft(0L)({case (a,b) => a+b.size}))

            }
        }
        return listCuboids

    }


    def resultExo() : Long = {
        val res = simulateReboot(inputList,(-50,50))
        //res.distinct.foreach(println(_))
        return res.foldLeft(0L)({case (a,b) => a+b.size})

    }


    def resultExoP2() : Long = {
        val res = simulateReboot(inputList,(0,0))
        //res.distinct.foreach(println(_))
        return res.foldLeft(0L)({case (a,b) => a+b.size})
    }

}


object Day22 extends App {
    
    val data = "data/day22_input.txt"
    val exo = new Day22(data)
    
    println(s"Exercice Day 22 : [${exo.resultExo}]")
    println(s"Exercice Day 22 Part2 : [${exo.resultExoP2}]")
    
}
 
//sbt :  ~runMain fr.pragmatias.Exo2021.Day22