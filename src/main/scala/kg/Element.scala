package kg

import scala.collection.mutable.HashMap

class Element(_id : Int) {

    val properties = HashMap[String,Primitive]("id"->id)

    def id = _id
    
    def getProperties() : HashMap[String,Primitive] = properties.clone()
    
    def setProperties(properties : HashMap[String,Primitive]) =
    {
       this.properties.addAll(properties)
    }

    def apply(property : String) : Primitive =
    {
        properties.get(property) match{
            case Some(i) => i
            case None   => throw new Exception("Property not found.")
        }
    } // apply

    override def toString() : String =
    {
        var str = ""
        val maxLength = properties.map( (k,v) => k.length).max + 1
        // TODO : FIND A PROPER MAX PROPERTY FIELD WIDTH VALUE INSTEAD OF 4
        var sep = "+" + ("-"*(maxLength+4) + "+") * properties.count( _ => true)
        str = str + sep + "\n+"
        var args = List[Any]()
        for (property <- properties) {
            val formatter = property._2 match{
                // TODO : Properly match cases...
                case _ => "d"
            }
            str = str + "%" + maxLength + "s : %" + formatter + "+"
            args = args :+ property._1  
            args = args :+ property._2 
        }
        str = str + "\n"
        str = str + sep
        return str.format(args:_*)
    }
}

class Node(id : Int) extends Element(id){}

class Edge(id : Int) extends Element(id){}

object ElementTester extends App{

    class Road(id : Int) extends Node(id){}
    class Road2(id : Int) extends Node(id){}
    class Intersection(id : Int) extends Edge(id){}
}