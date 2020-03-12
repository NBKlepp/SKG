package kg

import scala.reflect.ClassTag
import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scalation.util.ReArray
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
/*
 * The NodeType2 object holds a representation of all of the Nodes of this
 * NodeType as well as the meta information around this NodeType including
 * the properties that a Node of this NodeType can have, the domains for
 * those properties, and the EdgeTypes for which this NodeType can be
 * either a subject node or object node.
 * NodeType2 can be used with the Node2 object which keeps the properties
 * of the Node object as a map instead of a Vector so that each Node need
 * not store a value for ALL properties available to the NodeType, only
 * as many as needed. 
 * 
 */

abstract class NodeType {
    
    val schema = HashMap[String,PrimitiveType]()

    def addProperty(property : String, domain : PrimitiveType) =
    {
        schema.update(property,domain)
    }

    def getSchema() = schema.clone()
    
} // NodeType

object NodeTypeTester extends App{

    class Road extends NodeType{
        addProperty("name",StrO)
        addProperty("lanes",Int)
    }
    object Road extends GraphAlgebra
    
    val r1 = Road(HashMap[String,Primitive]("name"->1,"type"->4))
    val r2 = Road(HashMap[String,Primitive]("name"->2,"type"->3))
    val r3 = Road(HashMap[String,Primitive]("name"->3,"type"->2))
    val r4 = Road(HashMap[String,Primitive]("name"->3,"type"->1))
    
    val stuff = Road.select("name", _==3)
    println(s"stuff: ${stuff}")
    
} // NodeType2Tester