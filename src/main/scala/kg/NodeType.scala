package kg

import scala.collection.mutable.HashMap
import scala.collection.immutable.{Vector,Range}
        
trait NodeType(_name : String)  extends ElementTracker[Node]{

    var name = _name
    def reName(name : String) = {this.name = name}
    def getName() : String =  name

    override def getElements() : Vector[Node] =
        this.elements.foldLeft(Vector[Node]())((x,y) => x :+ y)
    
    def apply(properties : HashMap[String,Primitive]) : Node =
    {
        try{
            validate(properties)
            val node = new Node()
            node.setProperties(properties) 
            elements = elements :+ node
            //print(s"from apply, nodes: ${this}")
            node
        }catch{
            case e  : Exception =>
                {
                    println(s"WARNING: Invalid element for NodeType ${name}.")
                    println(s"${e.getMessage()}")
                    println("Node Not Added.")
                    throw new Exception()
                } // case e
        } // catch
    } // apply
} // NodeType