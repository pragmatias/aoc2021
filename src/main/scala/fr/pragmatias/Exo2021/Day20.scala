package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day20(inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)

    class Pixel(val char : Char) {
        def getBinary : String = char match {
            case '#' => "1"
            case _ => "0"
        }
    }

    class Image(val pixels : IndexedSeq[IndexedSeq[Pixel]], val pixelOutofImage : Pixel)  {
        val limitX = pixels(0).length
        val limitY = pixels.length

        def getPixel(x : Int, y : Int) : Pixel = {
            if (y >= limitY || y < 0 || x >= limitX || x < 0)  { pixelOutofImage } else { pixels(y)(x) }
        }

        def affiche() : Unit = {
            for (i <- 0 until limitY) {
                for ( j <- 0 until limitX) {
                    print(this.pixels(i)(j).char)
                }
                println
            }
        }

    }


    def pixelsToBinary(pixels : IndexedSeq[String]) : Int = Integer.parseInt(pixels.foldLeft("")(_+_),2)

    val dataAlgo : IndexedSeq[Pixel] = inputList(0).toIndexedSeq.map(x => new Pixel(x))
    val dataInput : Image = new Image((1 until inputList.size).toIndexedSeq.map(x => inputList(x).toIndexedSeq.map(y => new Pixel(y))),new Pixel('.'))

    def getAlgoRes(reference : Image, abs:Int, ord:Int) : Pixel = {
        val pixels = IndexedSeq((-1,-1),(-1,0),(-1,1),(0,-1),(0,0),(0,1),(1,-1),(1,0),(1,1)).map(x => reference.getPixel(x._2+abs,x._1+ord).getBinary)
        return dataAlgo(pixelsToBinary(pixels))
    }


    def simuleAlgo(image : Image) : Image = {
        val newImage : IndexedSeq[IndexedSeq[Pixel]] = {
            for ( y <- -1 until (image.limitY + 1) )
                yield
                    for ( x <- -1 until (image.limitX + 1) )
                        yield getAlgoRes(image,x,y)
        }
        val pixelOut = if (image.pixelOutofImage.char == '.') dataAlgo(0) else dataAlgo(511)
        return new Image(newImage,pixelOut)
    }

    def countingChar(image : Image) : Int = {
        return image.pixels.flatten.groupMapReduce(_.getBinary)(_ => 1)(_+_).filter(x => x._1 == "1").head._2
    }


    def resultExo() : Int = {
        val res = (1 to 2).foldLeft(dataInput)({case (a,_) => simuleAlgo(a)})
        return countingChar(res)
    }


    def resultExoP2() : Int = {
        val res = (1 to 50).foldLeft(dataInput)({case (a,_) => simuleAlgo(a)})
        return countingChar(res)
    }

}


object Day20 extends App {
    
    val data = "data/day20_input.txt"
    val exo = new Day20(data)
    
    println(s"Exercice Day 20 : [${exo.resultExo}]")
    println(s"Exercice Day 20 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day20