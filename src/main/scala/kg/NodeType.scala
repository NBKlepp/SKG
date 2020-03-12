package kg

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scalation.util.ReArray

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

abstract class NodeType(properties : HashMap[String,Primitive]) {
    
    import NodeType._

    val tracked = addNode(properties)
    
    /*  
    def getName() : String = name 

    //The schema (with domains) for nodes of this NodeType
    private var schema = new HashMap[String,PrimitiveType]()

    def this(name : String, schema : HashMap[String,PrimitiveType]) =
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
    
     *
     * The map for getting the property values of a node from the system id for the node
     * Note that you'll need the system id for the node which you'll probably need to get
     * from the idMap 
     *
    private val nodes = new HashMap[Int,Node]()
    
    //The EdgeTypes for which this node can be either a subject or object
    private var subjectEdges = Vector[EdgeType]()
    private var objectEdges  = Vector[EdgeType]()

     *
     * Get the meta information for this NodeType
     * TODO : Fix this to return a copy of the hashmap
     *
    def getSchema() = schema.clone()

     *
     * Get the number of nodes of this NodeType
     *
    def numNodes() = nNodes

     *
     * Get the number of properties for this NodeType
     *
    def numProperties() = nProp
    
     *
     * The getters for the types of edges that a node of this type can
     * be either a subject or object for
     *
    def isSubjectFor(et : EdgeType) = {subjectEdges.contains(et)}
    def isObjectFor (et : EdgeType) = {objectEdges.contains(et)}

     *
     * The setters for the types of edges that a node of this type can
     * be either a subject or object for      
     *
     *
    def makeSubjectFor(et : EdgeType) = {subjectEdges = subjectEdges :+ et}
    def makeObjectFor (et : EdgeType) = {objectEdges  = objectEdges :+ et}

     *
     * Add the property and it's domain to this NodeType
     *
     *
    def addProperty(propertyName : String, domain : PrimitiveType) =
    {
    	nProp += 1
	    schema.update(propertyName,domain)
    } // addProperty

    def getPropertyDomain(property : String) = schema.get(property)
     */
    
} // NodeType

object NodeType{
    
    private var nodes = Vector[HashMap[String,Primitive]]()

    def addNode(properties : HashMap[String,Primitive]) = {nodes = nodes :+ properties}

    def select(property : String, value : Primitive) = nodes.filter(x => x.getOrElse(property,null)==value) 
}

object NodeTypeTester extends App{

    class Road(properties : HashMap[String,Primitive]) extends NodeType(properties){}

    val r1 = Road(HashMap[String,Primitive]("name"->1,"type"->4))
    val r2 = Road(HashMap[String,Primitive]("name"->2,"type"->3))
    val r3 = Road(HashMap[String,Primitive]("name"->3,"type"->2))
    val r4 = Road(HashMap[String,Primitive]("name"->3,"type"->1))

    val stuff = Road.select("name",3)
    print(stuff)
    
} // NodeType2Tester