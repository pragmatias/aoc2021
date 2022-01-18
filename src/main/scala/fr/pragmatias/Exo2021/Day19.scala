package fr.pragmatias.Exo2021

import fr.pragmatias.Tools
import scala.collection.mutable.Map

class Day19 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    class Coord(val x : Int, val y : Int, val z : Int) {
        def distanceBetween( c : Coord ) : Distance = new Distance(this,c)
        def moveDistance(d : Distance) : Coord = new Coord(this.x + d.x,this.y + d.y,this.z + d.z)
        def calcManhattanDistance(c: Coord) : Int = (this.x - c.x).abs + (this.y - c.y).abs + (this.z - c.z).abs
        override def toString : String = s"(${x},${y},${z})"
    }
    //class Beacon(x : Int, y : Int, z : Int) extends Coord(x,y,z)
    //class Scanner(x : Int, y : Int, z : Int) extends Coord(x,y,z)
    class Distance(val c1 : Coord, val c2 : Coord) {
        var x : Int = c2.x - c1.x
        var y : Int = c2.y - c1.y
        var z : Int = c2.z - c1.z
        var value : Double = Math.sqrt(((c2.x - c1.x)*(c2.x - c1.x)) + ((c2.y - c1.y)*(c2.y - c1.y)) + ((c2.z - c1.z)*(c2.z - c1.z)))

        def this(x: Int, y: Int, z : Int) {
            // Invoking primary constructor
            this(new Coord(x,y,z),new Coord(x,y,z))
            this.x=x
            this.y=y
            this.z=z
        }

        def matching (d : Distance): Boolean = (value == d.value) && (
            (this.x.abs == d.x.abs && this.y.abs == d.y.abs && this.z.abs == d.z.abs) ||
            (this.x.abs == d.x.abs && this.y.abs == d.z.abs && this.z.abs == d.y.abs) ||
            (this.x.abs == d.y.abs && this.y.abs == d.x.abs && this.z.abs == d.z.abs) ||
            (this.x.abs == d.y.abs && this.y.abs == d.z.abs && this.z.abs == d.x.abs) ||
            (this.x.abs == d.z.abs && this.y.abs == d.x.abs && this.z.abs == d.y.abs) ||
            (this.x.abs == d.z.abs && this.y.abs == d.y.abs && this.z.abs == d.x.abs) )
        def updateDistance(x: Int, y : Int, z : Int) : Unit = {this.x = x ; this.y = y ; this.z = z }
    }


    class Direction(val d1 : Distance, val d2 : Distance) {
        var dir : Int = 0
        var coefX : Int = 1
        var coefY : Int = 1
        var coefZ : Int = 1
        var d2Distance : Distance = new Distance(0,0,0)

        if (d1.x.abs == d2.x.abs && d1.y.abs == d2.y.abs && d1.z.abs == d2.z.abs) { dir = 0 }
        else if (d1.x.abs == d2.x.abs && d1.y.abs == d2.z.abs && d1.z.abs == d2.y.abs) { dir = 1 }
        else if (d1.x.abs == d2.y.abs && d1.y.abs == d2.x.abs && d1.z.abs == d2.z.abs) { dir = 2 }
        else if (d1.x.abs == d2.y.abs && d1.y.abs == d2.z.abs && d1.z.abs == d2.x.abs) { dir = 3 }
        else if (d1.x.abs == d2.z.abs && d1.y.abs == d2.x.abs && d1.z.abs == d2.y.abs) { dir = 4 }
        else if (d1.x.abs == d2.z.abs && d1.y.abs == d2.y.abs && d1.z.abs == d2.x.abs) { dir = 5 }

        dir match {
            case 1 => d2Distance.updateDistance(d2.x,d2.z,d2.y)
            case 2 => d2Distance.updateDistance(d2.y,d2.x,d2.z)
            case 3 => d2Distance.updateDistance(d2.y,d2.z,d2.x)
            case 4 => d2Distance.updateDistance(d2.z,d2.x,d2.y)
            case 5 => d2Distance.updateDistance(d2.z,d2.y,d2.x)
            case _ => d2Distance.updateDistance(d2.x,d2.y,d2.z)
        }
        if (d1.x == -1*d2Distance.x && d1.x != 0) { coefX = -1 }
        if (d1.y == -1*d2Distance.y && d1.y != 0) { coefY = -1 }
        if (d1.z == -1*d2Distance.z && d1.z != 0) { coefZ = -1 }

        def getCoordRedirected (pos : Coord) : Coord = {
            var pTmp : Coord = new Coord(0,0,0)
            dir match {
                case 1 => pTmp = new Coord(pos.x, pos.z, pos.y)
                case 2 => pTmp = new Coord(pos.y, pos.x, pos.z)
                case 3 => pTmp = new Coord(pos.y, pos.z, pos.x)
                case 4 => pTmp = new Coord(pos.z, pos.x, pos.y)
                case 5 => pTmp = new Coord(pos.z, pos.y, pos.x)
                case _ => pTmp = new Coord(pos.x, pos.y, pos.z)
            }
            new Coord(pTmp.x * coefX,pTmp.y * coefY,pTmp.z * coefZ)
        }

        def isWellPositionned : Boolean = coefX == 1 && coefY == 1 && coefZ == 1 && dir == 0

        override def toString : String = s"[dir:${dir} --> (${coefX},${coefY},${coefZ})]"

    }


    class Scanner (val num : Int , var listCoord : List[Coord], val scannerCoord : Coord = new Coord(0,0,0)) {
        val listBeacon : List[(Coord,List[Distance])] = listCoord.map(x => (x,listCoord.filter(y => x != y).map(y => x.distanceBetween(y))))
        //var scannerCoord : Coord = new Coord(0,0,0)

        override def toString : String = s"(${num} -> ${this.listCoord})"
        def modifScannerDirection (dirs : List[Direction]) : Scanner = new Scanner(this.num,dirs.foldLeft(listBeacon.map(_._1))({case (list,dir) => list.map(c => dir.getCoordRedirected(c))}))
        def moveDistanceForAllBeacon(moves : List[Distance]) : Scanner = new Scanner(this.num,moves.foldLeft(listBeacon.map(_._1))({case (list,m) => list.map(c => c.moveDistance(m))}),moves.foldLeft(scannerCoord)({case (c,m) => c.moveDistance(m)}))
        def intersectListDistance(list1 : List[Distance], list2: List[Distance]) : List[Distance] = list1.filter(l1 => list2.exists(l2 => l1.matching(l2)))
        
        def getOverlappingPos(s : Scanner) : (List[(Coord,Coord)],Direction,Distance) = {
            val res = listBeacon.map({case (c,ld) => s.listBeacon.map({case (sc,sld) => (c,sc,intersectListDistance(ld,sld),intersectListDistance(sld,ld))}).maxBy(_._3.size)}).filter(_._3.nonEmpty)
            //println(s)
            //println(res)
            if (res.nonEmpty) {
                val dir = res.map({ case (a, b, c, d) => (a, b, c.map(x => new Direction(d.filter(y => y.matching(x)).head, x))) }).flatMap(x => x._3).groupBy(x => x.dir+","+x.coefZ+","+x.coefX+","+x.coefY).toList.maxBy(_._2.size)._2.head
                val dist = res.map({ case (a, b, _, _) => (a, b, a.distanceBetween(b)) }).map(x => x._3).groupBy(_.value).toList.maxBy(_._2.size)._2.head
                (res.map(x => (x._1,x._2)),dir,dist)
            } else {
                var dist0 = new Distance(new Coord(0,0,0),new Coord(0,0,0))
                (List(),new Direction(dist0,dist0),dist0)
            }

        }


        def getListDirectionsDist(listing : List[(Scanner,Scanner,Direction,Distance)], listScans : List[Int] = List(this.num), dirs : List[Direction] = List(),dists : List[Distance] = List()): (List[Int],List[Direction],List[Distance]) = {
            val listTmp = listing.filter(l => l._1.num == listScans.last && !listScans.contains(l._2.num))
            var res : List[(List[Int],List[Direction],List[Distance])] = List((listScans,dirs,dists))
            if (!listScans.contains(0) && listTmp.nonEmpty) {
                for(scan <- listTmp) {
                    res ::= (getListDirectionsDist(listing, listScans ::: List(scan._2.num), dirs ::: List(scan._3), dists:::List(scan._4)))
                }
            }
            if (!res.exists(x => x._1.contains(0))) {
                (listScans,dirs,dists)
            } else {
                res.filter(x => x._1.contains(0)).minBy(x => x._2.size)
            }
        }

    }

    def getScansOverlapping (scans : List[Scanner]) : List[(Scanner,Scanner,Direction,Distance)] = {
        scans.flatMap(x => scans.filter(y => x.num != y.num).map(y => (x,y,x.getOverlappingPos(y))).filter(_._3._1.size == 12)).map({case (a,b,c) => (a,b,c._2,c._3)})
        //res.foreach({case (a,b,c) => println(s"Scan : ${a.num} - Scan : ${b.num} ==> [${c._1.size}] : [${c._2}] --> [${c._3}]")})
    }





    def extractInput(content : List[String], resultScan : List[Scanner], numScan : Int = 0, resultList : List[Coord] = List()) : List[Scanner] = {
        if (content.isEmpty) {
            val res = resultScan ::: List(new Scanner(numScan,resultList))
            return res
        }
        val line = content.head
        if (line.contains("scanner")) {
            val numScanNew = line.split("scanner")(1).trim.split("\\s+")(0).toInt
            if (resultList.nonEmpty) {
                return extractInput(content.tail,resultScan ::: List(new Scanner(numScan,resultList)),numScanNew,List())
            } else {
                return extractInput(content.tail,resultScan,numScanNew,List())
            }
        } else {
            line match {
                case s"${a},${b},${c}" => return extractInput(content.tail,resultScan,numScan,resultList ::: List(new Coord(a.toInt,b.toInt,c.toInt)))
                case _ => throw new Exception("line don't match")
            }
        }
    }

    def scansRepositionning(scans : List[Scanner]) : List[Scanner] = {
        val scansMapping = getScansOverlapping(scans)
        val scansDirsToFirstScan = scans.map(x => (x,x.getListDirectionsDist(scansMapping))).map({case (a,(b,c,d)) => (a,c,d,b)})
        //scansDirsToFirstScan.foreach(x => println(s"${x._1.num} - ${x._4} - ${x._3}"))
        if (scansDirsToFirstScan.flatMap(x => x._2).exists(!_.isWellPositionned)) {
            // direction
            //println("dir")
            return scansRepositionning(scans.map(s => s.modifScannerDirection(scansDirsToFirstScan.filter(_._1.num == s.num).head._2)))
        } else {
            // distance
            //println("move")
            //scansDirsToFirstScan.map(x => (x._1.num,x._3.foldLeft(new Coord(0,0,0))({case (a,b) => a.moveDistance(b)}))).foreach(println(_))
            return scans.map(s => s.moveDistanceForAllBeacon(scansDirsToFirstScan.filter(_._1.num == s.num).head._3))
        }
    }

    def resultExo() : Int = {
        val scans = scansRepositionning(extractInput(inputList,List()))
        // take all points and group By
        val counting = scans.flatMap(scan => scan.listCoord).groupBy( x => (x.x,x.y,x.z)).size
        return counting
    }


    def resultExoP2() : Int = {
        val scans = scansRepositionning(extractInput(inputList,List()))
        //scans.foreach(x => println(s"[${x.num}] : ${x.scannerCoord}"))
        val manhattanDistMax = scans.flatMap(s1 => scans.filter(s2 => s1.num != s2.num).map(s2 => (s1.num,s2.num,s1.scannerCoord.calcManhattanDistance(s2.scannerCoord)))).maxBy(_._3)
        //println(manhattanDistMax)
        return manhattanDistMax._3

       // return 0
    }


}


object Day19 extends App {
    
    val data = "data/day19_input.txt"
    val exo = new Day19(data) 
    
    println(s"Exercice Day 19 : [${exo.resultExo}]")
    println(s"Exercice Day 19 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day19