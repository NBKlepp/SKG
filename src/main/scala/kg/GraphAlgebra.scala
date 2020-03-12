package kg
import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap

trait GraphAlgebra{

    var elements = Vector[HashMap[String,Primitive]]()
    
    def apply(element : HashMap[String,Primitive]) =
    {
        elements = elements :+ element
    }
    
    def select(property : String, p : (Primitive) => Boolean) = elements.filter( x => p(x(property)))

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