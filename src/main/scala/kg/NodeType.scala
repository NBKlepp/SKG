package knowledgeGraph

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO

/*
 * The NodeType object holds a representation of all of the Nodes of this
 * NodeType as well as the meta information around this NodeType including
 * the properties that a Node of this NodeType can have, the domains for
 * those properties, and the EdgeTypes for which this NodeType can be
 * either a subject node or object node. 
 * 
 */

class NodeType(name : String){

    /*
     *  Constructs an instance of NodeType with a full schema defined.
     *  schema should be a map of items (property -> domain)
     *
     */
    def this(name : String, schema : HashMap[String,PrimitiveType]) =
    {
        this(name)
        for (pd <- schema) addProperty(pd._1,pd._2)
    } // this
    
    
    def getName() = name

    //The number of properties for this NodeType
    var nProp = 0

    //The number of nodes of this NodeType
    var nNodes = 0 

    //The map for getting node system id numbers from node names
    private val idMap = new HashMap[String,Int]()

    //The map for getting the index of a property in the meta info vector from the property names
    private val propertyNameMap = new HashMap[String, Int]()

    //The row representation of the nodes of this type
    //private val rows = Vector[Complex | Rational | Real | StrO | StrNum | TimeO]()
    //private val rows = Vector[Complex | Rational | Real | StrNum ]()
    private val rows = Vector[Primitive]()
    
    //The columnar representation of thennodes of this NodeType
    //private val cols = Vector[Complex | Rational | Real | StrO | StrNum | TimeO]()
    //private val cols = Vector[Complex | Rational | Real | StrNum ]()
    private val cols = Vector[Primitive]()
    
    /*
     * The map for getting the property values of a node from the system id for the node
     * Note that you'll need the system id for the node which you'll probably need to get
     * from the idMap 
     */
    //private val nodes = new HashMap[Int,Vector[Complex | Rational | Real | StrO | StrNum | TimeO]]()
    //private val nodes = new HashMap[Int,Vector[Option[Complex | Rational | Real | StrNum ]]]()
    private val nodes = new HashMap[Int,Vector[Primitive]]()
    
    //The domains of the properties for nodes of this NodeType
    //A vector of domains, does not include the names of the properties
    //TODO : Should we do this with a hashmap instead so that every node of this type doesn't have to have every property?
    private var domains = Vector[PrimitiveType]()

    //The properties for nodes of this NodeType
    //A vector of property names, does not include the domains of the properties
    private var properties = Vector[String]()

    //The schema for nodes of this type. 
    private val schema = new HashMap[String,PrimitiveType]()
    
    //The EdgeTypes for which this node can be either a subject or object
    private var subjectEdges = Vector[EdgeType]()
    private var objectEdges  = Vector[EdgeType]()

    /*
     * Get the domains information for this NodeType
     * TODO : Fix this to return a copy of the array
     */
    def getDomains() = domains ++ Vector[PrimitiveType]()

    def getProperties() = domains ++ Vector[String]

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
    def addProperty(propertyName : String, domain : PrimitiveType) =
    {
    	propertyNameMap.update(propertyName, nProp)
        schema.update(propertyName,domain)
	    domains = domains :+ domain
        properties = properties :+ propertyName 
        nProp += 1
    } // addProperty

    /*
     *  Get the domain for a specific property for this NodeType
     */
    def getPropertyDomain(property : String) : Option[String] = schema.get(property)

    def getPropertyDomain(i : Integer) : String = domains(i)

    def addNode(node : Node) =
    {
        val sid = nNodes
        nNodes += 1
        nodes.update(sid,node.getProperties())
        sid 
    }

    //def updateNode(sid : Integer, pName : String, property : Complex | Rational | Real | StrNum ) : Option[String] =
    def updateNode(sid : Integer, pName : String, property : Primitive ) : Option[String] =
    {
        schema.get(pName) match{
            case Some(domain)    =>  {
                                    //HEEEEEEEEEEEEEEEEEEEEEEEEEEERE
                                    if ( expected == passed ) {
                                        var properties = nodes(sid)
                                        properties = properties.updated(i,Some(property))
                                        nodes -= sid
                                        nodes.update(sid,properties)
                                        None
                                    } // if
                                    else Some(s"PROPERTY VALUE DOMAIN MISMATCH. Expected: ${expected}. Passed: ${passed}") 
                                } // case Some(i)
            case None       =>  Some(s"NO PROPERTY ${pName} FOUND")     
        } // match
    } // updateNode

    def schema() : String =
    {
        var schema = "["
        val it = propertyNameMap.iterator
        for (kv <- it){
            var pName = kv._1
            var domain = domains(kv._2)
            schema = schema + s"${pName}::${domain}${if (it.hasNext) "," else ""}"
        } // for
        schema += "]"
        schema
    } // schema
} // NodeType


object NodeTypeTester extends App{
    val pass = "Pass"
    val fail = "Fail"

    val roadProperties = new HashMap[String,String]
    val road       = new NodeType("road")
    val sensor     = new NodeType("sensor")
    val intersects = new EdgeType("intersects")
    val measures   = new EdgeType("measures")

    println("Testing associations for empty NodeTypes")     

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

    println(s"\tDomains information for road before adding properties: ${road.getDomains()}")
    println(s"\tDomains information for sensor before adding properties: ${sensor.getDomains()}")

    println()
    
    def afterProp(nt : NodeType,property : String) = {
        println(s"\tAfter adding ${property} property to ${nt.getName()} NodeType:")
        println(s"\t\tdomains: ${nt.getDomains()}")
        println(s"\t\tnumProperties: ${nt.numProperties()}")
        //println(s"\t\ttesting the propertyNameMap: ${if (nt.getDomains(nt.propertyNameMap(property)).equals(name)) pass else fail}")
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
        
} // NodeTypeTester

object NodeTypeTester2 extends App{
    val pass = "Pass"
    val fail = "Fail"

    val roadProperties = HashMap[String,String](
                            "name"->"String",
                            "type"->"String",
                            "district"->"int")

    val sensorProperties = HashMap[String,String](
                                "id"->"int",
                                "district"->"int")

    def afterConstruct(nt : NodeType) = {
        println(s"\tAfter constructor for ${nt.getName()}:")
        println(s"\t\tdomains: ${nt.getDomains()}")
        println(s"\t\tnumProperties: ${nt.numProperties()}")
    }
    
    val road       = new NodeType("road",roadProperties)
    val sensor     = new NodeType("sensor",sensorProperties)

    println(s"road schema : ${road.schema()}")
    println(s"sensor schema : ${sensor.schema()}")
}

