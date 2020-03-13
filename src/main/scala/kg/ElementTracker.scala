package kg

import scala.collection.mutable.HashMap
import scala.collection.immutable.{Vector,Range}

trait ElementTracker{    

    var elements = Vector[HashMap[String,Primitive]]()
    val schema = HashMap[String,PrimitiveType]()
    
    def apply(element : HashMap[String,Primitive]) =
    {
        elements = elements :+ element
    }

    def addProperty(property : String, domain : PrimitiveType) =
    {
        schema.update(property,domain)
    }

    def getSchema() = schema.clone()

    def getElements() = elements ++ Vector[HashMap[String,Primitive]]()

    override def toString() =
    {
        var str = ""
        val maxLength = schema.keys.foldLeft( 0 )( (x,y) => {if (x>=y.length) x else y.length} ) + 2
        val keyIndexMap = HashMap[Int,String]()
        val keyIndices = (schema.keys zip Range(0,schema.size))
        keyIndices.foreach(x => keyIndexMap.update(x._2,x._1))
        for (ki <- keyIndices) str = str + f"|${ki._1}%10s|"
        str = str + "\n"
        for (element <- elements) {
            for (i <- Range(0,keyIndexMap.size)) {
                val property = keyIndexMap(i)
                element.get(property) match{
                    case None       => str += f"|${" "}%10s|"
                    case Some(i)    => str += f"|${" "}%9s${i}|"
                }
            }
            str += "\n"
        }
        str
    }

    /*  
    
    //The EdgeTypes for which this node can be either a subject or object
    private var subjectEdges = Vector[EdgeType]()
    private var objectEdges  = Vector[EdgeType]()

     *
     * Get the number of nodes of this NodeType
     *
    def numNodes() = {}

     *
     * Get the number of properties for this NodeType
     *
    def numProperties() {}
    
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

    def getPropertyDomain(property : String) = schema.get(property)
     */
}