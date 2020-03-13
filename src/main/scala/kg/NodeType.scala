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

abstract class NodeType {} // NodeType

object NodeTypeTester extends App{

    class Road extends NodeType{}
    object Road extends GraphAlgebra

    class Road2 extends NodeType{}
    object Road2 extends GraphAlgebra

    class Sensor extends NodeType{}
    object Sensor extends GraphAlgebra

    Road.addProperty("name",Int)
    Road.addProperty("lanes",Int)
    Road.addProperty("type",Int)

    Road2.addProperty("name",Int)
    Road2.addProperty("lanes",Int)
    Road2.addProperty("type",Int)
    
    Sensor.addProperty("id",Int)
    Sensor.addProperty("district",Int)
    
    val r1 = Road(HashMap[String,Primitive]("name"->1,"type"->4,"lanes"->1))
    val r2 = Road(HashMap[String,Primitive]("name"->2,"type"->3,"lanes"->2))
    val r3 = Road(HashMap[String,Primitive]("name"->3,"type"->2,"lanes"->3))
    val r4 = Road(HashMap[String,Primitive]("name"->3,"type"->1,"lanes"->4))

    val r5 = Road2(HashMap[String,Primitive]("name"->1,"type"->4,"lanes"->1))
    val r6 = Road2(HashMap[String,Primitive]("name"->3,"type"->2,"lanes"->3))
    val r7 = Road2(HashMap[String,Primitive]("name"->5,"type"->2,"lanes"->3))
    
    val s1 = Sensor(HashMap[String,Primitive]("id"->1,"district"->4))
    val s2 = Sensor(HashMap[String,Primitive]("id"->2,"district"->3))
    val s3 = Sensor(HashMap[String,Primitive]("id"->3,"district"->2))
    val s4 = Sensor(HashMap[String,Primitive]("id"->4,"district"->1))
    
    println(s"Road: \n${Road}")
    println()
    println(s"Sensor: \n${Sensor}")
    println()
    println(s"Road2: \n${Road2}")
    println()
    
    val Sel = Road.select("name", _==3)
    println(s"select for name == 3: \n${Sel}")
    println()
    
    val Proj = Road.project("name","type")
    println(s"Road.project on name and type: \n${Proj}")
    println()
    
    val Union = Road.union(Sensor)
    println(s"Road union Sensor: \n${Union}")
    println()
    
    val Inter = Road.intersect(Road2)
    println(s"Road intersect Road2: \n${Inter}")
    println()
    
    val Diff = Road.minus(Road2)
    println(s"Road minus Road2: \n${Diff}")
    
} // NodeType2Tester