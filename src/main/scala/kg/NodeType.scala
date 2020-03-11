package knowledgeGraph

    /*
        QUESTIONS :
            Should we handle ensuring that some properties are transitive? reflexive? commutative? reflexive?
    */

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scala.reflect._
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

class NodeType2(name : String){
      
    def getName() : String = name 

    //The schema (with domains) for nodes of this NodeType
    private var schema = new HashMap[String,String]()

    def this(name : String, schema : HashMap[String,String]) =
    {
        this(name)
        this.schema = schema.clone()
    }
    
    //The number of properties for this NodeType
    var nProp = 0

    //The number of nodes of this NodeType
    var nNodes = 0 

    //The map for getting node system id numbers from node names
    private val idMap = new HashMap[String,Int]()

    /*
     * The map for getting the property values of a node from the system id for the node
     * Note that you'll need the system id for the node which you'll probably need to get
     * from the idMap 
     */
    private val nodes = new HashMap[Int,Node]()
    
    //The EdgeTypes for which this node can be either a subject or object
    private var subjectEdges = Vector[EdgeType]()
    private var objectEdges  = Vector[EdgeType]()

    /*
     * Get the meta information for this NodeType
     * TODO : Fix this to return a copy of the hashmap
     */
    def getSchema() = schema.clone()

    /*
     * Get the number of nodes of this NodeType
     */
    def numNodes() = nNodes

    /*
     * Get the number of properties for this NodeType
     */
    def numProperties() = nProp
    
    /*
     * The getters for the types of edges that a node of this type can
     * be either a subject or object for
     */
    def isSubjectFor(et : EdgeType) = {subjectEdges.contains(et)}
    def isObjectFor (et : EdgeType) = {objectEdges.contains(et)}

    /*
     * The setters for the types of edges that a node of this type can
     * be either a subject or object for      
     *
     */
    def makeSubjectFor(et : EdgeType) = {subjectEdges = subjectEdges :+ et}
    def makeObjectFor (et : EdgeType) = {objectEdges  = objectEdges :+ et}

    /*
     * Add the property and it's domain to this NodeType
     *
     */
    def addProperty(propertyName : String, domain : String) =
    {
    	nProp += 1
	    schema.update(propertyName,domain)
    } // addProperty

    def getPropertyDomain(property : String) = schema.get(property)
    
} // NodeType
    
object NodeType2Tester extends App{
    val pass = "Pass"
    val fail = "Fail"

    val road       = new NodeType2("road")
    val sensor     = new NodeType2("sensor")
    val intersects = new EdgeType("intersects")
    val measures   = new EdgeType("measures")

    println("Testing associations for empty NodeType2s")     

    println(s"\troad is not a subject for intersects: ${if (!road.isSubjectFor(intersects)) pass else fail}")
    println(s"\troad is not an object for intersects: ${if (!road.isObjectFor(intersects)) pass else fail}")
    println(s"\troad is not a subject for measures: ${if (!road.isSubjectFor(measures)) pass else fail}")
    println(s"\troad is not an object for measures: ${if (!road.isObjectFor(measures)) pass else fail}")
    println(s"\tsensor is not a subject for intersects: ${if (!sensor.isSubjectFor(intersects)) pass else fail}")
    println(s"\tsensor is not an object for intersects: ${if (!sensor.isObjectFor(intersects)) pass else fail}")
    println(s"\tsensor is not a subject for measures: ${if (!sensor.isSubjectFor(measures)) pass else fail}")
    println(s"\tsensor is not an object for measures: ${if (!sensor.isObjectFor(measures)) pass else fail}")

    println(s"adding associations...")
    road.makeSubjectFor(intersects)
    road.makeObjectFor(intersects)
    sensor.makeSubjectFor(measures)
    road.makeObjectFor(measures)

    println("testing associations...")
    println(s"\troad is a subject for intersects: ${if (road.isSubjectFor(intersects)) pass else fail}")
    println(s"\troad is an object for intersects: ${if (road.isObjectFor(intersects)) pass else fail}")
    println(s"\troad is not a subject for measures: ${if (!road.isSubjectFor(measures)) pass else fail}")
    println(s"\troad is an object for measures: ${if (road.isObjectFor(measures)) pass else fail}")
    println(s"\tsensor is not a subject for intersects: ${if (!sensor.isSubjectFor(intersects)) pass else fail}")
    println(s"\tsensor is not an object for intersects: ${if (!sensor.isObjectFor(intersects)) pass else fail}")
    println(s"\tsensor is a subject for measures: ${if (sensor.isSubjectFor(measures)) pass else fail}")
    println(s"\tsensor is not an object for measures: ${if (!sensor.isObjectFor(measures)) pass else fail}")
    
    println("Testing properties...")

    println(s"\tSchema information for road before adding properties: ${road.getSchema()}")
    println(s"\tSchema information for sensor before adding properties: ${sensor.getSchema()}")

    println()
    
    def afterProp(nt : NodeType2,property : String) = {
        println(s"\tAfter adding ${property} property to ${nt.getName()} NodeType2:")
        println(s"\t\tschema: ${nt.getSchema()}")
        println(s"\t\tnumProperties: ${nt.numProperties()}")
        //println(s"\t\ttesting the propertyNameMap: ${if (nt.getSchema(nt.propertyNameMap(property)).equals(name)) pass else fail}")
    }

    road.addProperty("name","String")
    afterProp(road,"name")

    road.addProperty("type","String")
    afterProp(road,"type")

    road.addProperty("district","int")
    afterProp(road,"district")
    
    sensor.addProperty("id","int")
    afterProp(sensor,"id")
    
    sensor.addProperty("district","int")
    afterProp(sensor,"district")
        
} // NodeType2Tester