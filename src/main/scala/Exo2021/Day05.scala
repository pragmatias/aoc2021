package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Plateau(x : Int, y : Int) {
    val sizeX = this.x+1
    val sizeY = this.y+1

    val plateau = Array.ofDim[Int](sizeY,sizeX)

    override def toString() : String = {
        var s = s"Plateau : (${sizeY},${sizeX})\n"
        for (i <- 0 until sizeY) {
            s+="   "
            for (j <- 0 until sizeX) {
                s += plateau(i)(j)
            }
            s+="\n"
        } 
        return s
    }

    def applyListVent (lv : List[Vent]) : Unit = {
        lv.foreach(this.applyVent(_))
    }

    def applyVent ( v : Vent ) : Unit = {
        val (x1,y1) = v.coordFrom.extractCoord
        val (x2,y2) = v.coordTo.extractCoord
        val diffx = x1 - x2
        val diffy = y1 - y2
        // si c'est X à gérer
        if (diffx != 0) {
            // si la diff est positive
            if (diffx < 0) {
                for (i <- x1 to x2) {
                    plateau(y1)(i) += 1
                }
            } else {
            // si la diff est negative
                for (i <- x2 to x1) {
                    plateau(y1)(i) += 1
                }
            }
        } else if (diffy != 0) {
            if (diffy < 0) {
                for (i <- y1 to y2) {
                    plateau(i)(x1) += 1
                }
            } else {
                for (i <- y2 to y1) {
                    plateau(i)(x1) += 1
                }
            }
        }
        //println(this)
    }

    def countOverlap : Int = {
        var cpt : Int = 0
        for (i <- 0 until sizeY) {
            for (j <- 0 until sizeX) {
                if (plateau(i)(j) > 1 ) {cpt +=1 }
            }
        }
        return cpt
    }

}


class Point (x : Int, y : Int) extends AnyRef {
    val abs : Int = this.x
    val ord : Int = this.y

    def extractCoord : (Int,Int) = {
        return (abs,ord)
    }

    def canEqual(a: Any) = a.isInstanceOf[Point]

    override def equals(that: Any): Boolean =
        that match {
            case that: Point => {
                that.canEqual(this) &&
                (abs == that.abs && ord == that.ord)
            }
            case _ => false
        }

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + abs;
        result = prime * result + ord;
        return result
    }

    override def toString : String = {
        return s"(${abs},${ord})"
    }
}

class Vent(coord : String) {
    
    val patternVent = "^([0-9]+),([0-9]+)\\s+->\\s+([0-9]+),([0-9]+)".r
    val patternVent(x1,y1,x2,y2) = coord
    val coordFrom : Point = new Point(x1.toInt,y1.toInt)
    val coordTo : Point = new Point(x2.toInt,y2.toInt)


    var listPoints : List[Point] = List()

    def hasSameXorY : Boolean = {
        return (coordFrom.abs == coordTo.abs) || (coordFrom.ord == coordTo.ord)
    }

    def isHoriVertDiag : Boolean = {
        val diffx = (coordFrom.abs - coordTo.abs).abs
        val diffy = (coordFrom.ord - coordTo.ord).abs
        return hasSameXorY || (diffx == diffy)
    }

    def maxX : Int = {
        return if (coordFrom.abs > coordTo.abs) {coordFrom.abs} else {coordTo.abs}
    }

    def maxY : Int = {
        return if (coordFrom.ord > coordTo.ord) {coordFrom.ord} else {coordTo.ord}
    }

    def generatedAllPoints : Vent = {
        val (x1,y1) = coordFrom.extractCoord
        val (x2,y2) = coordTo.extractCoord
        val diffx = (x2 - x1)
        val diffy = (y2 - y1)
        val diffxAbs = diffx.abs
        val diffyAbs = diffy.abs
        // si c'est diagonal
        if (diffxAbs == diffyAbs && diffx != 0 && diffy != 0)  {
            // les 2 sont positifs
            if (diffx > 0 && diffy > 0) {
                for (i <- 0 to diffxAbs) {
                    listPoints ::= new Point(x1+i,y1+i)
                }
                // les 2 sont positifs
            } else if (diffx < 0 && diffy < 0 ) {
                for (i <- 0 to diffxAbs) {
                    listPoints ::= new Point(x2+i,y2+i)
                }
                // x est positif et y est negatif
            } else if (diffx > 0 && diffy < 0 ) {
                for (i <- 0 to diffxAbs) {
                    listPoints ::= new Point(x1+i,y1-i)
                }
                // x est négatif et y est positif
            } else {
                 for (i <- 0 to diffxAbs) {
                    listPoints ::= new Point(x1-i,y1+i)
                }
            }
            // si c'est X à gérer
        } else if (diffy == 0 && diffx != 0) {
            // si la diff est positive
            if (diffx < 0) {
                for (i <- x2 to x1) {
                    listPoints ::= new Point(i,y1)
                }
            } else {
            // si la diff est negative
                for (i <- x1 to x2) {
                    listPoints ::= new Point(i,y1)
                }
            }
        } else if (diffx == 0 && diffy != 0) {
            if (diffy < 0) {
                for (i <- y2 to y1) {
                    listPoints ::= new Point(x1,i)
                }
            } else {
                for (i <- y1 to y2) {
                    listPoints ::= new Point(x1,i)
                }
            }
        }
    
        return this
    }

    override def toString : String = {
        return s"${coordFrom} -> ${coordTo}"
    }
}



class Day05 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)


    def resultExo() : Int = {
        val listVent = inputList.map(x => new Vent(x)).filter(_.hasSameXorY)
        //listVent.foreach(println(_))
        val maxSizeX = listVent.map(_.maxX).max
        val maxSizeY = listVent.map(_.maxY).max
        var plat = new Plateau(maxSizeX,maxSizeY)
        plat.applyListVent(listVent)
        //println(s"\n${plat}")

        return plat.countOverlap
    }


    def resultExoP2() : Int = {
        val listVent : List[Point]= inputList.map(x => new Vent(x)).filter(_.isHoriVertDiag).flatMap(_.generatedAllPoints.listPoints)
        //println(listVent)
        val test = listVent.groupMapReduce(identity)(_ => 1)(_+_).toList.filter(_._2 > 1)
        //println(test)
        return test.size
    }

}


object Day05 extends App {
    
    val data = "data/day05_input.txt"
    val exo = new Day05(data) 
    
    println(s"Exercice Day 05 : [${exo.resultExo}]")
    println(s"Exercice Day 05 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day05