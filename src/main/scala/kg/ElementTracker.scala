package kg

import scala.collection.mutable.HashMap
import scala.collection.immutable.{Vector,Range}

class InvalidDomainException(message : String) extends Exception(message)

class InvalidPropertyException(message : String) extends Exception(message)
    
trait ElementTracker{    

    var elements = Vector[HashMap[String,Primitive]]()
    val schema = HashMap[String,PrimitiveType]()
    
    def apply(element : HashMap[String,Primitive]) =
    {
        try{
            validDomains(element) 
            elements = elements :+ element
        }catch{
            case ipe : InvalidPropertyException =>  {
                                                        println(s"WARNING : ${ipe.getMessage()}")
                                                        println(s"Elemented Not Added.")
                                                    }
            case ide : InvalidDomainException   =>  {
                                                        println(s"WARNING: ${ide.getMessage()}")
                                                        println(s"Element Not Added.")
                                                    }
        }
    }

    /*
     * Get the number of properties for this ElementTracker object
     *
     */
    def schemaSize = schema.size

    /*
     * Get the number of nodes for this ElementTracker object.
     *
     */
    def elementsSize = elements.size
    
    def addProperty(property : String, domain : PrimitiveType) =
    {
        schema.update(property,domain)
    }

    def getSchema() = schema.clone()

    def getElements() = elements ++ Vector[HashMap[String,Primitive]]()

    def validDomains(element : HashMap[String,Primitive]) =
    {
        for ( pv <- element ) {

            val property    = pv._1
            val value       = pv._2
     
            schema.get(property) match {
                case Some(i)    =>  {
                                        val expected = (i.getClass).
                                                        toString.
                                                        replace("$","").
                                                        replace("class scala.","")
                                        val passed = (value.getClass).
                                                        toString.
                                                        replace("class java.lang.","").
                                                        replace("Integer","Int")
                                        if ( expected != passed ) {
                                            val message = s"""Error in creating new element:
                                                            |${prettyElement(element)}
                                                            |Value ${value} for ${property} has invalid data type.
                                                            |type ${expected} expected, type ${passed} passed.""".stripMargin
    /*
                                            var message = s"Invalid value type passed for property ${property}. "
                                            message = message + s"Expected type: ${expected}. "
                                            message = message + s"Type passed: ${passed}. "
    */
                                            throw new InvalidDomainException(message)
                                        } // if
                                    } // Some(i)
                case None       =>  throw new InvalidPropertyException("Invalid property for NodeType : ${property}") 
            } // match
        } // for
    } // valiDomains
    
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

    // TODO Fix formatting for different data types
    private def prettyElement(element : HashMap[String,Primitive]) : String =
    {
        var top = ""
        var bot = ""
        for ( pv <- element ) {
            top  = top + f"${pv._1}%10s"
            bot = bot + f"${" "}%9s" + s"${pv._2}"
        }
        top + "\n" + bot
    }
    /*

    //The ElementTracker objects this ElementTracker is related to 
    private var subjectElements = Vector[T <: ElementTracker]()
    private var objectElements  = Vector[T <: ElementTracker]()
    
    //The EdgeTypes for which this node can be either a subject or object
    private var subjectEdges = Vector[EdgeType]()
    private var objectEdges  = Vector[EdgeType]()
    
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