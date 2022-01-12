package fr.pragmatias.Exo2021

import fr.pragmatias.Tools


class Day16 (inputData : String) {
  
    val inputList : List[(String)] = new Tools.FileInput(this.inputData).listContent.filterNot(_.trim.isEmpty)


    def HexaToBinary(c : Char) : String = {
        return c match {
            case '0' => "0000"
            case '1' => "0001"
            case '2' => "0010"
            case '3' => "0011"
            case '4' => "0100"
            case '5' => "0101"
            case '6' => "0110"
            case '7' => "0111"
            case '8' => "1000"
            case '9' => "1001"
            case 'A' => "1010"
            case 'B' => "1011"
            case 'C' => "1100"
            case 'D' => "1101"
            case 'E' => "1110"
            case 'F' => "1111"
        }
    }

    def translateHexaToBinary(input : String) : String = {
        return input.map(c => HexaToBinary(c)).foldLeft("")(_+_)
    }

    def hexaToInt(hexa : String) : Int = {
        return Integer.parseInt(hexa,2)
    }

    def hexaToLong(hexa : String) : Long = {
        return java.lang.Long.parseLong(hexa,2)
    } 
    
    class Packet (version : Int, typeId : Int) {
        def getVersion : Int = { return version }
        def sumVersion : Int = { return getVersion }
        def Operation : Long = { return 0L}
        override def toString : String = {
            return s"Version:[${version}] - Type:[${typeId}]"
        } 
    }  

    class PacketOpe(version : Int, typeId : Int, contentPacket : List[Packet] ) extends Packet(version,typeId) {
        override def toString : String = {
            val str = contentPacket.map(x => "   "+x.toString+"\n").foldLeft("\n")(_+_)
            return s"Version:[${version}] - Type:[${typeId}] - Value:[${str}]"
        }

        override def Operation : Long = {
            typeId match {
                case 0 => return contentPacket.map(_.Operation).sum
                case 1 => return contentPacket.map(_.Operation).foldLeft(1L)(_*_)
                case 2 => return contentPacket.map(_.Operation).min
                case 3 => return contentPacket.map(_.Operation).max
                case 5 => return if (contentPacket(0).Operation > contentPacket(1).Operation) 1L else 0L
                case 6 => return if (contentPacket(0).Operation < contentPacket(1).Operation) 1L else 0L
                case 7 => return if (contentPacket(0).Operation == contentPacket(1).Operation) 1L else 0L
                case _ => return 0L
            }
        }

        override def sumVersion : Int = {
            return contentPacket.map(x => x.sumVersion).sum + version
        }
    }
    
    class PacketLit(version : Int, typeId : Int,  content : Long) extends Packet(version,typeId) {
        override def toString : String = {
            return s"Version:[${version}] - Type:[${typeId}] - Value:[${content}]"
        }

        override def Operation : Long = {
            return content
        }
    }

    def listAllPackets(input: String, listPacket: List[Packet], step : Int) : (List[Packet],String) = {
        if (input.size <= 10 || step == 0) { return (listPacket,input) }
        val version = hexaToInt(input.substring(0,3))
        val typeid = hexaToInt(input.substring(3,6))
        
        typeid match {
            case 4 => {
                        val t = buildPacketType4(version,typeid,input.substring(6))
                        return listAllPackets(t._2,listPacket:::t._1,step+1)
                    } 
            case _ => {
                        val l = input.substring(6,7)
                        l match {
                            case "0" => { // type != 4 and len = 15
                                val ll = buildPacketNotType4Len15(version,typeid,input.substring(7))
                                return listAllPackets(ll._2,listPacket:::ll._1,step+1)
                            }
                            case _ => { // type != 4 and len = 11
                                val ll = buildPacketNotType4Len11(version,typeid,input.substring(7))
                                return listAllPackets(ll._2,listPacket:::ll._1,step+1)
                            }
                        }
            }
        }

        return (listPacket,input)
    }


    def buildPacketType4(version : Int, typeId : Int, input: String) : (List[Packet],String) = {
        var num = 0
        var valueString = "" 
        while(input.substring(num,num+1).toInt == 1) {
            valueString += input.substring(num+1,num+5)
            num += 5
        }
        valueString += input.substring(num+1,num+5)
        num += 5
        //if (num%4 > 0) { num += (4 - (num % 4)) }

        return (List(new PacketLit(version,typeId,hexaToLong(valueString))),input.substring(num))
    }

    def buildPacketNotType4Len15(version : Int, typeId : Int, input: String) : (List[Packet],String) = {
        val lenSub = 15
        var labelSize = hexaToInt(input.substring(0,lenSub))
        val t = listAllPackets(input.substring(lenSub,lenSub+labelSize),List(),1)
        return (List(new PacketOpe(version,typeId,t._1)),input.substring(lenSub+labelSize))
    }

    def buildPacketNotType4Len11(version : Int, typeId : Int, input: String) : (List[Packet],String) = {
        val lenSub = 11
        var labelSize = hexaToInt(input.substring(0,lenSub))
        val t = listAllPackets(input.substring(lenSub),List(),-labelSize)
        return (List(new PacketOpe(version,typeId,t._1)),t._2)
    }


    def readInputBinary(input : String) : Int = {
        val t = listAllPackets(input,List(),1)
        //t._1.foreach(println(_))
        return t._1.map(x => x.sumVersion).sum
    }

    def resultExo() : Int = {
        //println(inputList(0))
        val res = listAllPackets(translateHexaToBinary(inputList(0)),List(),1)
        //println(test)
        return res._1.map(x => x.sumVersion).sum
    }


    def resultExoP2() : Long = {
        //println(inputList(0))
        val res = listAllPackets(translateHexaToBinary(inputList(0)),List(),1)
        return  if (res._1.size == 1) res._1.head.Operation else 0L
       
    }

}


object Day16 extends App {
    
    val data = "data/day16_input.txt"
    val exo = new Day16(data) 
    
    println(s"Exercice Day 16 : [${exo.resultExo}]")
    println(s"Exercice Day 16 Part2 : [${exo.resultExoP2}]")
    
}

//sbt :  ~runMain fr.pragmatias.Exo2021.Day16