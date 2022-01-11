package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day15 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    case class Coord(x:Int,y:Int)

    class Plateau(content : List[String]) {
        val limitY = content.size
        val limitX = content(0).size
        val plateau : Array[Array[Int]] = Array.ofDim[Int](limitY,limitX)
        for ( i <- 0 until limitX ) {
            for ( j <- 0 until limitY) {
                plateau(i)(j) = content(i).substring(j,j+1).toInt
            }
        }

        def costCoord(node : Coord) : Long = {
            return plateau(node.x)(node.y).toLong
        }

        def pathIsEnded(path : List[(Int,Int)]) : Boolean = {
            return path.filter(x => x._1 == (limitX -1) && x._2 == (limitY -1)).size == 1
        }

        def findNextCoord(node : Coord) : List[Coord] = {
            return List((0,1),(1,0),(0,-1),(-1,0)).map(x => new Coord(x._1+node.x,x._2+node.y))
                                                  .filter(x => x.x >= 0 && x.x < limitX && x.y >= 0 && x.y < limitY)
        }

        def findLowestCostPathStart(start : Coord, end : Coord) : (Long,List[Coord]) = {
            val nodeVisited = collection.mutable.Map.empty[Coord,Long]

            def costOrder(d: (Long,Coord)) : Long = -d._1
            val queueNodeToVisite = collection.mutable.PriorityQueue.empty[(Long,Coord)](Ordering.by(costOrder))
            
            var currentNode = start
            var currentCost = 0L
            nodeVisited.addOne(currentNode,currentCost)
            queueNodeToVisite ++= findNextCoord(start).map(x=> (costCoord(x),x))

            while (!queueNodeToVisite.isEmpty) {
                val (queueHeadCost,queueHeadCoord) = queueNodeToVisite.dequeue

                if (!nodeVisited.contains(queueHeadCoord) 
                    ||((nodeVisited.contains(queueHeadCoord) && nodeVisited.getOrElse(queueHeadCoord,queueHeadCost) > queueHeadCost))) {

                    nodeVisited.addOne(queueHeadCoord,queueHeadCost)
                    for (nodeCoord <- findNextCoord(queueHeadCoord)) {
                        val nodeCost = queueHeadCost+costCoord(nodeCoord)
                        if (!nodeVisited.contains(nodeCoord)) {
                            queueNodeToVisite.enqueue((nodeCost,nodeCoord))
                        }
                    }
                    
                } //else {
                    queueNodeToVisite.dropWhile(x => x._2.x == queueHeadCoord.x && x._2.y == queueHeadCoord.y)
                //}
                val c1 = nodeVisited.size
                val c2 = queueNodeToVisite.size
                println(s"Status : Visited [${c1}] - Queued [${c2}]")
            }
            //println(nodeVisited)

            return (nodeVisited.getOrElse(end,0L),getCompletePath(start,end,nodeVisited))
        }


        def getCompletePath(start : Coord, end : Coord, mapping : collection.mutable.Map[Coord,Long]) : List[Coord] = {
            var currentCost = mapping.getOrElse(end,0L)
            var listPaths = List(end)
            while (!listPaths.contains(start)) {
               listPaths = findNextCoord(listPaths.head).map(x => (mapping.getOrElse(x,0L),x)).filter(x => x._1 < currentCost || x._1 == 0).sortBy(x => x._1).head._2 :: listPaths
            }
            return listPaths
        }



        def affichePlateau : Unit = {
            for ( i <- 0 until limitX ) {
                for ( j <- 0 until limitY) {
                    print(plateau(i)(j))
                }
                println
            }
        }
    }

    def resultExo() : Long = {
        val p = new Plateau(inputList)
        //p.affichePlateau
        val res = p.findLowestCostPathStart(new Coord(0,0),new Coord(p.limitX-1,p.limitY-1))
        println(res._2)
        return res._1
        //return 0L
    }


    def resultExoP2() : Long = {
        //val inputListX5 = (1 until 5).foldLeft(inputList)({case (input,num) => input.map( line => line + line.map( c => c.toString.toInt + num).map( c => if (c >= 10) { c - 9 } else { c } ).foldLeft("")(_+_.toString))})
        val inputListTmp : List[String] = inputList.map( l => (1 until 5).foldLeft(l)({case (line,num) => line + l.map( c => c.toString.toInt + num).map( c => if (c >= 10) { c - 9 } else { c } ).foldLeft("")(_+_.toString)}))
        val inputListX5 : List[String]  = (1 until 5).foldLeft(inputListTmp)({case (listLine,num) => listLine ::: inputListTmp.map(l => l.map( c => c.toString.toInt + num).map( c => if (c >= 10) { c - 9 } else { c }).foldLeft("")(_+_.toString))})
        val p = new Plateau(inputListX5)
        //p.affichePlateau
        val res = p.findLowestCostPathStart(new Coord(0,0),new Coord(p.limitX-1,p.limitY-1))
        println(res._2)
        return res._1
        //return 0
    }

}


object Day15 extends App {
    
    val data = "data/day15_input.txt"
    val exo = new Day15(data) 
    
    println(s"Exercice Day 15 : [${exo.resultExo}]")
    println(s"Exercice Day 15 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day15