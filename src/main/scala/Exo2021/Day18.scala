package fr.pragmatias.Exo2021

import fr.pragmatias.Tools
import scala.annotation.tailrec


class Day18 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    trait Tree
    class Branch (val left : Tree, val right : Tree) extends Tree {
        override def toString : String = {
            return s"[${left},${right}]"
        }
    }
    class Leaf (val num : Int) extends Tree {
        override def toString : String = {
            return s"${num}"
        }
    }
    

    class Snailfish(var content : String,var tr : Tree) {
        var snailfish : Tree = if (content == "") tr else extractContent(content)

        def this(content : String) {
            this(content,new Leaf(0))
        }
        def this(tr : Tree) {
            this("",tr)
        }

        private def extractContent(content : String) : Tree = {
            //println(content)
            val c = content.substring(0,1)
            c match {
                case "[" => {
                        val t = searchFirstBranch(content,0,List(),List())
                        return new Branch(extractContent(content.substring(t._1+1,t._2)),extractContent(content.substring(t._2+1,t._3)))
                }
                case  _ => return new Leaf(c.toInt)
            }
        }
     

        @tailrec
        final def searchFirstBranch(content : String, cursor : Int, deb : List[Int], sep : List[Int]) : (Int,Int,Int) = {
            if (content.length <= cursor) { return (0,0,0) }
            val c = content.substring(cursor,cursor + 1)
            c match {
                case "[" => return searchFirstBranch(content,cursor+1,deb:::List(cursor),sep)
                case "]" => {
                        if (deb.size == 1) {
                            return (deb.head,sep.head,cursor)
                        } else {
                            return searchFirstBranch(content,cursor+1,deb.dropRight(1),sep.dropRight(1))
                        }                
                    }  
                case "," => return searchFirstBranch(content,cursor+1,deb,sep:::List(cursor))
                case _ => return searchFirstBranch(content,cursor+1,deb,sep)
            }
            
        }


        def add(s : Snailfish) : Snailfish = {
            return reduce(new Branch(snailfish,s.snailfish))
        }


        def reduce(snail : Tree) : Snailfish = {
            val listTmp : List[(Tree,String,Int)] = List()
            var trTmp : Tree = snail
            var cpt = 0
            var listing = listReduceAction(trTmp).foldLeft(listTmp)({case (l,r) => cpt+=1 ; (r._1,r._2,cpt)::l}).sortBy(_._3)
            listing = listing.filter(_._2 == "Explode") ::: listing.filter(_._2 != "Explode")
            //println(trTmp) 
            //println(listing)
            while (listing.size > 0) {
                val tmp = listing.head
                //println(s" ToDo : [${listing.head}]")
                if (tmp._2 == "Explode") {
                    trTmp = reduceExplode(trTmp,tmp._1,0,0,0,false)._1
                } else {
                    trTmp = reduceSplit(trTmp,tmp._1)
                }
                //println(s" Res --> ${trTmp}")
                cpt = 0
                listing = listReduceAction(trTmp).foldLeft(listTmp)({case (l,r) => cpt+=1 ; (r._1,r._2,cpt)::l}).sortBy(_._3)
                listing = listing.filter(_._2 == "Explode") ::: listing.filter(_._2 != "Explode")
                //println(listing)
            }                

            //println(s" Res --> ${trTmp}")
            return new Snailfish(trTmp) 
        }

        // se ballader dans l'arbre ...
        def reduceExplode (tr : Tree, temoin : Tree, niv : Int, vl : Int, vr : Int, red : Boolean) : (Tree,Int,Int,Boolean) = {
            tr match {
                case b : Branch => {
                        if (niv > 3 && temoin == b) {
                            val (trLeft,vlRes,_,_) = reduceExplode(b.left,temoin,niv+1,0,0,false)
                            val (trRight,_,vrRes,_) = reduceExplode(b.right,temoin,niv+1,0,0,false)
                            //println(s" --> "+b+" ::: "+niv+" -- L="+vlRes+" -- R="+vrRes)
                            return (new Leaf(0),vlRes,vrRes,true)
                        } else {
                            val (trLeft,vlRes,vrRes,bl) = reduceExplode(b.left,temoin,niv+1,0,vr,false)
                            val (trRight,vlRes2,vrRes2,br) = reduceExplode(b.right,temoin,niv+1,vl,0,false)
                            //println(s" --> "+b+" --- "+niv+" -- L="+vlRes+"|"+bl+" -- R="+vrRes+"|"+br)
                            if (bl) {
                                //println("branche right")
                                return (new Branch(trLeft,reduceExplode(b.right,temoin,niv+1,0,vrRes,true)._1),vlRes,0,true)
                            } else if (br) {
                                //println("branche left")
                                return (new Branch(reduceExplode(b.left,temoin,niv+1,vlRes2,0,true)._1,trRight),0,vrRes2,true)
                            } else  {
                                //println("branche autres")
                                return (new Branch(trLeft,trRight),vl,vr,red)
                            }
                        }
                    }
                case l : Leaf => { 
                    //println("Leaf-"+l.num+"-"+vl+"-"+vr+"-")
                    return (new Leaf(l.num+vl+vr),l.num,l.num,false)
                }
                
            }
        }


        def reduceSplit (tr : Tree, temoin : Tree) : Tree = {
            tr match {
                case b : Branch => new Branch(reduceSplit(b.left,temoin),reduceSplit(b.right,temoin))
                case l : Leaf => { 
                    if (l == temoin) {
                        val left = (l.num.toDouble / 2.0).toInt // round Down
                        val right = (l.num.toDouble / 2.0).ceil.toInt // round Up
                        new Branch(new Leaf(left),new Leaf(right))
                    } else {
                        l 
                    }
                    //println("Leaf-"+l.num+"-"+vl+"-"+vr+"-")
                    //return (new Leaf(l.num+vl+vr),l.num,l.num,false)
                }
                
            }
        }
 

        def listReduceAction (tr : Tree, niv : Int = 0) : List[(Tree,String)] = {
            return tr match {
                case a : Leaf => if ( a.num > 9 ) List((a,"Split"))  else  List() 
                case b : Branch => 
                    if (niv > 3) {
                        List((tr,"Explode"))
                    } else {
                        listReduceAction(b.left,niv+1) ::: listReduceAction(b.right,niv+1)
                    }
            }
        }

        def magnitude(tr : Tree = snailfish) : Long = {
            tr match {
                case b: Branch => 3L*(magnitude(b.left)) + 2L*(magnitude(b.right))
                case l: Leaf => l.num.toLong
            }
        }

        def affiche() : Unit = {
            println(snailfish)
        }

        override def toString : String = {
            return snailfish.toString
        }
        

    }


    def maxMagnitudeFound(snailFishs : List[Snailfish]) : Long = {
        return snailFishs.map(x => snailFishs.filter(y => y != x).map(y => x.add(y).magnitude()).max).max
    }



    def resultExo() : Long = {
        val listSnail = inputList.map(new Snailfish(_))
        //listSnail.foreach(x => println("Input : "+x))
        val listSnailResult = listSnail.tail.foldLeft(listSnail.head)({case (a,b) => a.add(b)})
        //println("Result : "+listSnailResult)
        return listSnailResult.magnitude() 
    }



    def resultExoP2() : Long = {
        val listSnail = inputList.map(new Snailfish(_))
        return maxMagnitudeFound(listSnail)
    }

}


object Day18 extends App {
    
    val data = "data/day18_input.txt"
    val exo = new Day18(data) 
    
    println(s"Exercice Day 18 : [${exo.resultExo}]")
    println(s"Exercice Day 18 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day18