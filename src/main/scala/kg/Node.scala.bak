package knowledgeGraph

import scala.collection.mutable.HashMap
import scala.collection.immutable.Vector
    
class Node(id : String, nt : NodeType) extends Elem(id) {

    def getId() = id

    def nodeType() = nt.getName()
    
    
    def this(id : String,nt : NodeType, _properties : Vector[Primitive]) =
    {
        this(id,nt)
        var properties = Vector[Primitive]()
        for (p <- 0 until _properties.length){
            val t1 = _properties(p).getClass().toString
            val t2 = nt.getPropertyDomain(p) 
            if ( t1 == t2 ) properties = properties.updated(p,_properties(p))
            else {
                println(s"Warning : incompatible property and domain:")
                println(s"\t${t1},${t2}.")
                println(s"Node ${id} not added...")
            }
        } // for
        this.properties = properties
    } // this
} 