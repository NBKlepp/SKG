package knowledgeGraph

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
    
class EdgeType(_name : String){

    private val name = _name
    def getName() : String = name

    //The number of properties for this EdgeType
    var nProp = 0

    //The number of edges of this EdgeType
    var nEdges = 0 

    //The map for getting node system id numbers from node names
    private val idMap = new HashMap[String,Int]()

    //The map for getting the index of a property in the meta info vector from the property names
    private val propertyNameMap = new HashMap[String, Int]()

    //The row representation of the edges of this type
    //private val rows = Vector[Complex | Rational | Real | StrO | StrNum | TimeO ]()
    private val rows = Vector[Complex | Rational | Real | StrNum ]()

    //The columnar representation of then edges of this EdgeType
    //private val cols = Vector[Complex | Rational | Real | StrO | StrNum | TimeO]()
    private val cols = Vector[Complex | Rational | Real | StrNum ]()

    /*
     * The map for getting the property values of a node from the system id for the node
     * Note that you'll need the system id for the node which you'll probably need to get
     * from the idMap 
     */
    //private val edges = new HashMap[Int,Vector[Complex | Rational | Real | StrO | StrNum | TimeO]]()
    private val edges = new HashMap[Int,Vector[Complex | Rational | Real | StrNum ]]()

    //The schema (with domains) for edges of this EdgeType
    private var meta = Vector[String]()

    //The EdgeTypes for which this node can be either a subject or object
    private var subjectNodes = Vector[NodeType]()
    private var objectNodes  = Vector[NodeType]()

    /*
     * Get the meta information for this EdgeType
     * TODO : Fix this to return a copy of the array
     */
    def getMeta() = meta

    /*
     * Get the number of edges of this EdgeType
     */
    def numEdges() = nEdges

    /*
     * Get the number of properties for this EdgeType
     */
    def numProperties() = nProp
    
    /*
     * The getters for the types of edges that a node of this type can
     * be either a subject or object for
     */
    def canLeadFrom(nt : NodeType) = subjectNodes.contains(nt)
    def canLeadTo  (nt : NodeType) = objectNodes.contains(nt)

    /*
     * The setters for the types of edges that a node of this type can
     * be either a subject or object for      
     *
     */
   
    def letLeadFrom(nt : NodeType) = {subjectNodes = subjectNodes :+ nt}
    def letLeadTo  (nt : NodeType) = {objectNodes  = objectNodes :+ nt}
    
    /*
     * Add the property and it's domain to this EdgeType
     *
     */
    def addProperty(propertyName : String, domain : String) =
    {
    	nProp += 1
    	propertyNameMap.update(propertyName, nProp)
	    meta = meta :+ domain
    }

} // EdgeType

object EdgeType{
    
}
object EdgeTypeTester extends App{
    val pass = "Pass"
    val fail = "Fail"

    val road       = new NodeType("road")
    val sensor     = new NodeType("sensor")
    val intersects = new EdgeType("intersects")
    val measures   = new EdgeType("measures")

    println("Testing associations for empty EdgeTypes")     

    println(s"\tintersects can not lead from road: ${if (!intersects.canLeadFrom(road)) pass else fail}")
    println(s"\tmeasures can not lead from road: ${if (!measures.canLeadFrom(road)) pass else fail}")
    println(s"\tintersects can not lead from sensor: ${if (!intersects.canLeadFrom(sensor)) pass else fail}")
    println(s"\tmeasures can not lead from sensor: ${if (!measures.canLeadFrom(sensor)) pass else fail}")
    println(s"\tintersects can not lead to road: ${if (!intersects.canLeadTo(road)) pass else fail}")
    println(s"\tmeasures can not lead to road: ${if (!measures.canLeadTo(road)) pass else fail}")
    println(s"\tintersects can not lead to sensor: ${if (!intersects.canLeadTo(sensor)) pass else fail}")
    println(s"\tmeasures can not lead to sensor: ${if (!measures.canLeadTo(sensor)) pass else fail}")

    println(s"Making associations...")
    
    measures.letLeadFrom(sensor)
    measures.letLeadTo(road)
    intersects.letLeadFrom(road)
    intersects.letLeadTo(road)

    println("Testing associations...")
   
    println(s"\tmeasures leads from sensor: ${if (measures.canLeadFrom(sensor)) pass else fail}")
    println(s"\tmeasures leads from road: ${if (!measures.canLeadFrom(road)) pass else fail}")
    println(s"\tmeasures leads to sensor: ${if (!measures.canLeadTo(sensor)) pass else fail}")
    println(s"\tmeasures leads to road: ${if (measures.canLeadTo(road)) pass else fail}")

    println(s"\tintersects leads from sensor: ${if (!intersects.canLeadFrom(sensor)) pass else fail}")
    println(s"\tintersects leads from road: ${if (intersects.canLeadFrom(road)) pass else fail}")
    println(s"\tintersects leads to sensor: ${if (!intersects.canLeadTo(sensor)) pass else fail}")
    println(s"\tintersects leads to road: ${if (intersects.canLeadTo(road)) pass else fail}")
    
    println("Testing add properties.")

    println(s"\tMeta information for measures before adding properties: ${measures.getMeta()}")
    println(s"\tMeta information for intersects before adding properties: ${intersects.getMeta()}")

    println()
    
    def afterProp(et : EdgeType, property : String) = {
        println(s"\tAfter adding ${property} property to ${et.getName()} EdgeType:")
        println(s"\t\tmeta: ${et.getMeta()}")
        println(s"\t\tnumProperties: ${et.numProperties()}")
        //println(s"\t\ttesting the propertyNameMap: ${if (nt.getMeta(nt.propertyNameMap(property)).equals(name)) pass else fail}")
    }

    measures.addProperty("frequency","String")
    afterProp(measures,"frequency")

    intersects.addProperty("lat","real")
    afterProp(intersects,"lat")

    intersects.addProperty("lon","real")
    afterProp(intersects,"lon")
        
} // EdgeTypeTester