package fr.pragmatias.Exo2021

import fr.pragmatias.Tools
import scala.collection.mutable.Map

class Day19 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    class Pos(val x : Int, val y : Int, val z : Int) {
        def distanceBetween( p : Pos ) : Dist = {
            return new Dist(p.x - this.x, p.y - this.y, p.z - this.z)
        }

        def move(dist : Dist) : Pos = {
            return new Pos(this.x - dist.x,this.y - dist.y,this.z - dist.z)
        }

        def manhattanDist(pos: Pos) : Int = {
            return (this.x-pos.x) + (this.y-pos.y) + (this.z-pos.z)
        }

        def isEquals(d : Pos) : Boolean = {
            return (this.x == d.x && this.y == d.y && this.z == d.z)
        }

        def canEqual(a: Any) = a.isInstanceOf[Pos]
        override def equals(that : Any) : Boolean = {
            that match {
                case that : Pos => {
                    that.canEqual(this) && isEquals(that)
                }
                case _ => false
            }
        }
        override def hashCode : Int = {
            val prime = 14
            var result = 1
            result = prime * result + x
            result = prime * result + y
            result = prime * result + z
            return result
        }

        override def toString : String = {
            return s"(${x},${y},${z})"
        }
    }

    class Dist(val x : Int, val y : Int, val z : Int) {
        // rotation & orientation
        def isEquals(d : Dist) : Boolean = {
            return (this.x.abs == d.x.abs && this.y.abs == d.y.abs && this.z.abs == d.z.abs) || 
                    (this.x.abs == d.x.abs && this.y.abs == d.z.abs && this.z.abs == d.y.abs) || 
                    (this.x.abs == d.y.abs && this.y.abs == d.x.abs && this.z.abs == d.z.abs) || 
                    (this.x.abs == d.y.abs && this.y.abs == d.z.abs && this.z.abs == d.x.abs) || 
                    (this.x.abs == d.z.abs && this.y.abs == d.y.abs && this.z.abs == d.x.abs) || 
                    (this.x.abs == d.z.abs && this.y.abs == d.x.abs && this.z.abs == d.y.abs)
        }

        def canEqual(a: Any) = a.isInstanceOf[Dist]
        override def equals(that : Any) : Boolean = {
            that match {
                case that : Dist => {
                    that.canEqual(this) && isEquals(that)
                }
                case _ => false
            }
        }
        override def hashCode : Int = {
            val prime = 16
            val result = 1
            return prime * result + x.abs + y.abs + z.abs
        }
        
        override def toString : String = {
            return s"{${x},${y},${z}}"
        }

    }

    class Direction(val d1 : Dist, val d2 : Dist) {
        var dir : Int = 0
        var coefX : Int = 1
        var coefY : Int = 1
        var coefZ : Int = 1

        if (d1.x.abs == d2.x.abs && d1.y.abs == d2.y.abs && d1.z.abs == d2.z.abs) { dir = 0 }
        else if (d1.x.abs == d2.x.abs && d1.y.abs == d2.z.abs && d1.z.abs == d2.y.abs) { dir = 1 }
        else if (d1.x.abs == d2.y.abs && d1.y.abs == d2.x.abs && d1.z.abs == d2.z.abs) { dir = 2 }
        else if (d1.x.abs == d2.y.abs && d1.y.abs == d2.z.abs && d1.z.abs == d2.x.abs) { dir = 3 }
        else if (d1.x.abs == d2.z.abs && d1.y.abs == d2.x.abs && d1.z.abs == d2.y.abs) { dir = 4 }
        else if (d1.x.abs == d2.z.abs && d1.y.abs == d2.y.abs && d1.z.abs == d2.x.abs) { dir = 5 }
        var d2Dir = new Dist(d2.x,d2.y,d2.z)
        dir match {
            case 1 => d2Dir = new Dist(d2.x,d2.z,d2.y)
            case 2 => d2Dir = new Dist(d2.y,d2.x,d2.z)
            case 3 => d2Dir = new Dist(d2.y,d2.z,d2.x)
            case 4 => d2Dir = new Dist(d2.z,d2.x,d2.y)
            case 5 => d2Dir = new Dist(d2.z,d2.y,d2.x)
            case _ => d2Dir = new Dist(d2.x,d2.y,d2.z)
        }
        if (d1.x == -1*d2Dir.x && d1.x != 0) { coefX = -1 }
        if (d1.y == -1*d2Dir.y && d1.y != 0) { coefY = -1 }
        if (d1.z == -1*d2Dir.z && d1.z != 0) { coefZ = -1 }

        def getPosRedirected (pos : Pos) : Pos = {
            var pTmp : Pos = new Pos(0,0,0)
            dir match {
                case 1 => pTmp = new Pos(pos.x, pos.z, pos.y)
                case 2 => pTmp = new Pos(pos.y, pos.x, pos.z)
                case 3 => pTmp = new Pos(pos.y, pos.z, pos.x)
                case 4 => pTmp = new Pos(pos.z, pos.x, pos.y)
                case 5 => pTmp = new Pos(pos.z, pos.y, pos.x)
                case _ => pTmp = new Pos(pos.x, pos.y, pos.z)
            }
            return new Pos(pTmp.x * coefX,pTmp.y * coefY,pTmp.z * coefZ)
        }

        def isWellPositionned : Boolean = {
            return coefX == 1 && coefY == 1 && coefZ == 1 && dir == 0
        }

        override def toString : String = {
            return s"[dir:${dir} --> (${coefX},${coefY},${coefZ})]"
        }
 
    }

    class Scan (val num : Int , val listPos : List[Pos]) {
        val listPosDist : List[(Pos,List[Dist])] = listPos.map(x => (x,listPos.filter(y => x != y).map(y => x.distanceBetween(y))))
        var listPosRedirected : List[Pos] = listPos
        var listPosRedirectedDist : List[(Pos,List[Dist])] = listPosDist
        var scanPos : Pos = new Pos(0,0,0)

        def applyListDirections(listingDirs : List[Direction]) : Scan = {
            for (d <- listingDirs) {
                //println(s"listdirect : ${this.num} : ${d}")
                listPosRedirected = listPosRedirected.map(x => d.getPosRedirected(x))
            }
            listPosRedirectedDist = listPosRedirected.map(x => (x,listPosRedirected.filter(y => x != y).map(y => x.distanceBetween(y))))
            return this
        }

        def applyListDistanceOnAllPos(moves : List[Dist]) : Scan = {
            scanPos = moves.foldLeft(scanPos)({ case (a,b) => a.move(b)})
            for (m <- moves) {
                listPosRedirected = listPosRedirected.map(x => x.move(m))
            }
            listPosRedirectedDist = listPosRedirected.map(x => (x,listPosRedirected.filter(y => x != y).map(y => x.distanceBetween(y))))
            return this
        }
        
        def getOverlappingPos( s : Scan ) : (List[(Pos,Pos)],Direction,Dist) = {
            val res = listPosRedirectedDist.map({case (p,l) => s.listPosRedirectedDist.map({case (ps,ls) => (p,ps,l.intersect(ls),ls)}).maxBy(_._3.size)}).filter(_._3.nonEmpty)
            if (res.isEmpty) { return (List(),new Direction(new Dist(0,0,0),new Dist(0,0,0)),new Dist(0,0,0)) }
            val dir = res.map({ case (a,b,c,d) => (a,b,c.map(x => new Direction(d.filter(y => y==x).head,x )))}).flatMap(x => x._3).groupBy(identity).toList.maxBy(_._2.size)._1
            val dist = res.map({ case (a,b,_,_) => (a,b,b.distanceBetween(a))}).map(x => x._3).groupBy(identity).toList.maxBy(_._2.size)._1
            //println(dir)
            return (res.map(x => (x._1,x._2)),dir,dist)
        }

        def getListDirectionsDist(listing : List[(Int,Int,Direction,Dist)], listScans : List[Int] = List(this.num), dirs : List[Direction] = List(),dists : List[Dist] = List()): (List[Direction],List[Dist]) = {
            if (listScans.contains(0)) {
                    return (dirs,dists)
            }
            val tmp = listing.filter(x => x._1 == listScans.last && !listScans.contains(x._2))
            if (tmp.nonEmpty) {
                var resTmp : (List[Direction],List[Dist]) = (List(),List())
                for( num <- tmp) {
                    resTmp = getListDirectionsDist(listing, listScans ::: List(num._2), dirs ::: List(num._3), dists:::List(num._4))
                    if (resTmp._1.nonEmpty) { return resTmp }
                }
                return (List(),List())
            } else {
                return (List(),List())
            }
        }

        override def toString : String = {
            return s"(${num} -> ${listPosRedirected})"
        }


    }

    def getScanOverlapping (scans : List[Scan]) : List[(Scan,Scan,List[(Pos,Pos)],Direction,Dist)] = {
        val res = scans.flatMap(x => scans.filter(y => x.num != y.num).map(y => (x,y,x.getOverlappingPos(y))).filter(_._3._1.size == 12))
        //res.foreach({case (a,b,c) => println(s"Scan : ${a.num} - Scan : ${b.num} ==> [${c._1.size}] : [${c._2}] --> [${c._3}]")})
        return res.map({case (a,b,c) => (a,b,c._1,c._2,c._3)})
    }



    def extractInput(content : List[String], resultScan : List[Scan], numScan : Int = 0, resultList : List[Pos] = List()) : List[Scan] = {
        if (content.isEmpty) {
            val res = resultScan ::: List(new Scan(numScan,resultList))
            return res
        }
        val line = content.head
        if (line.contains("scanner")) {
            val numScanNew = line.split("scanner")(1).trim.split("\\s+")(0).toInt
            if (resultList.nonEmpty) {
                return extractInput(content.tail,resultScan ::: List(new Scan(numScan,resultList)),numScanNew,List())
            } else {
                return extractInput(content.tail,resultScan,numScanNew,List())
            }
        } else {
            line match {
                case s"${a},${b},${c}" => return extractInput(content.tail,resultScan,numScan,resultList ::: List(new Pos(a.toInt,b.toInt,c.toInt)))
                case _ => throw new Exception("line don't match")
            }
        }
    }

    def scansPositionnement(scans : List[Scan]) : List[Scan] = {
        var scansRecadr = scans
        var scansDirs = getScanOverlapping(scansRecadr).map({case (a,b,_,dir,dist) => (a.num,b.num,dir,dist)})
        var scansDirsToFirstScan = scans.map(x => (x,x.getListDirectionsDist(scansDirs)))
        //scansDirsToFirstScan.foreach(x => println(s"TEST 1 : ${x._1.num} : ${x._2._1} - ${x._2._2}"))
        // Rotation & more
        while (scansDirsToFirstScan.flatMap(x => x._2._1).exists(!_.isWellPositionned)) {
            scansRecadr = scans.map(x => x.applyListDirections(scansDirsToFirstScan.filter(y => y._1.num == x.num).head._2._1))
            scansDirs = getScanOverlapping(scansRecadr).map({case (a,b,_,dir,dist) => (a.num,b.num,dir,dist)})
            scansDirsToFirstScan = scans.map(x => (x,x.getListDirectionsDist(scansDirs)))
        }
        // move all positions
        //scansDirsToFirstScan.foreach(x => println(s"TEST 2 : ${x._1.num} : ${x._2._1} - ${x._2._2}"))
        scansRecadr = scansRecadr.map(x => x.applyListDistanceOnAllPos(scansDirsToFirstScan.filter(y => y._1.num == x.num).head._2._2))

        return scansRecadr
    }

    def resultExo() : Int = {
        val scans = scansPositionnement(extractInput(inputList,List()))
        // take all points and group By
        val counting = scans.flatMap(x => x.listPosRedirected).groupBy(identity).size
        return counting
    }


    def resultExoP2() : Int = {
        val scans = scansPositionnement(extractInput(inputList,List()))
        scans.foreach(x => println(s"[${x.num}] : ${x.scanPos}"))
        val manhattandistMax = scans.flatMap(x => scans.filter(y => x.num != y.num).map(y => (x.num,y.num,x.scanPos.manhattanDist(y.scanPos)))).maxBy(_._3)
        println(manhattandistMax)
        return manhattandistMax._3
    }


}


object Day19 extends App {
    
    val data = "data/day19_input_test.txt"
    val exo = new Day19(data) 
    
    println(s"Exercice Day 19 : [${exo.resultExo}]")
    println(s"Exercice Day 19 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day19