package knowledgeGraph

import scala.collection.mutable.HashMap
import scala.collection.immutable.Vector
import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
    
class Node2(id : String, nt : NodeType) extends Elem(id) {

    def getId() = id

    def nodeType() = nt.getName()
    
    //def this(id : String,nt : NodeType, _properties : Vector[Complex | Rational | Real | StrO | StrNum | TimeO]) =
    def this(id : String,nt : NodeType, _properties : Vector[Complex | Rational | Real | StrNum ]) =
    {
        this(id,nt)
        var properties = Vector[Complex | Rational | Real | StrNum ]()
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