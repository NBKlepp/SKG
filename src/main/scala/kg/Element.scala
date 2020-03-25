package kg

import scala.collection.mutable.HashMap

class Element {

    var label : String = null
    def getLabel() : Option[String] =
    {
        if (label == null) return None
        else return Some(label)
    }
    
    
    def setLabel(label : String) = {this.label = label}
    
    val properties = HashMap[String,Primitive]()

    def getProperties() : HashMap[String,Primitive] = properties.clone()

    def apply(property : String) : Primitive =
    {
        properties.get(property) match{
            case Some(i) => i
            case None   => throw new Exception("Property not found.")
        }
    } // apply

    def copy() : Element =
    {
        val e = new Element()
        e.setProperties(properties)
        return e    
    }
    
    def setProperties(properties : HashMap[String,Primitive]) =
    {
        if (properties != null){
            this.properties.clear()
            this.properties.addAll(properties)
        } // if
    }

    def isType(et : ElementType) : Boolean = et.contains(this)
        
    
}

class Node extends Element{
    
}
class Edge extends Element{
    var edgeType : EdgeType = null
    def makeType(edgeType : EdgeType) = {this.edgeType = edgeType}
}

object ElementTester extends App{

    class Road extends Node{}
    class Road2 extends Node{}
    class Intersection extends Edge{}
        
}