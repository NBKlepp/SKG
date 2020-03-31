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

    def getOrElse(property : String, value : Primitive) : Primitive =
    {
        properties.get(property) match{
            case Some(i) => return i
            case None => return value
        }
    }

    def iterator = properties.iterator
        
    override def toString() : String =
    {
        var str = "+"
        val _properties = properties.toList
        var border = "+"
        for (property <- _properties) {
            border = border + "-" * ( property._1.length + (s"${property._2}").length + 5) + "+"
            str = str + s" ${property._1} : ${property._2} +"
        } // for
        return border + "\n" + str + "\n" + border
    }
    
}

class Node(id : Int) extends Element(id){}

class Edge(id : Int) extends Element(id){}

object ElementTester extends App{

    class Road(id : Int) extends Node(id){}
    class Road2(id : Int) extends Node(id){}
    class Intersection(id : Int) extends Edge(id){}
}