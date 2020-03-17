package kg

import scala.collection.mutable.HashMap
import scala.collection.immutable.{Vector,Range}

class InvalidDomainException(message : String) extends Exception(message)
class InvalidPropertyException(message : String) extends Exception(message)
class PrimaryKeyException(message : String) extends Exception(message)
class EmptyPrimaryKeyException(message : String) extends Exception(message)
    
trait ElementTracker(name : String) {    

    var elements = Vector[HashMap[String,Primitive]]()
    val schema = HashMap[String,PrimitiveType]()
    var primaryKey = Vector[String]()
    
    def apply(element : HashMap[String,Primitive]) =
    {
        try{
            validate(element) 
            elements = elements :+ element
        }catch{
            case e  : Exception =>  {
                                        println(s"WARNING: Invalid element for ElementTracker ${name}.")
                                        println(s"${e.getMessage()}")
                                        println("Element Not Added.")
                                    } // case e
        } // catch
    } // apply

    /*
     * Get the number of properties for this ElementTracker object
     *
     */
    def schemaSize = schema.size

    def getName() : String = name
    
    /*
     * Get the number of nodes for this ElementTracker object.
     *
     */
    def elementsSize = elements.size

    def addProperty(property : String, domain : PrimitiveType) =
    {
        schema.update(property,domain)
    }

    def addProperty( pd : Tuple2[String,PrimitiveType]) : Unit = 
    {
        addProperty(pd._1,pd._2)
    }

    def addPrimaryKey( property : String ) =
    {
        schema.get(property) match {
            case Some(i)     => primaryKey = primaryKey :+ property
            case None        => println(s"""WARNING: {property} not a property for element type ${name}
                                            |{property} not added as primary key property.""".stripMargin)
        }
    }

    def getSchema() = schema.clone()

    def getPrimaryKey() = primaryKey ++ Vector[String]()

    def getElements() = elements ++ Vector[HashMap[String,Primitive]]()

    def validate(element : HashMap[String,Primitive]) =
    {
        try{
            validatePrimaryKey(element)
            for ( pv <- element ) {
                val property    = pv._1
                val value       = pv._2
                schema.get(property) match {
                    case Some(i)    =>  validatePropertyValueDomain(i,property,value) 
                    case None       =>  throw new Exception(s"Property: ${property} not in schema for element type ${name}") 
                } // match
            } // for
        }catch{
            case e : Exception => throw new Exception(s"""${prettyElement(element)} 
                                                      |${e.getMessage()}""".stripMargin)
        }
    } // valiDomains

    private def validatePropertyValueDomain(i : PrimitiveType, property: String, value : Primitive) =
    {
        val expected = (i.getClass).
                          toString.
                          replace("$","").
                          replace("class scala.","")
        val passed = (value.getClass).
                          toString.
                          replace("class java.lang.","").
                          replace("Integer","Int")
        if ( expected != passed ) throw new Exception(s"""Value ${value} for ${property} has invalid data type.
                                                      |Type ${expected} expected, type ${passed} passed.""".stripMargin)
    }

    private def validatePrimaryKey(element : HashMap[String,Primitive]) =
    {
        if ( primaryKey.size == 0 ) throw new Exception("No primary key defined for element type {name}.")
        for ( k <- primaryKey if !element.keySet.contains(k) )
            throw new Exception(s"Missing primary key value for ${k} property of element type {name}")
    }
    
    override def toString() =
    {
        var str = s"${name}:\n"
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