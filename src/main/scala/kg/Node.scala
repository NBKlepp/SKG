package knowledgeGraph

import scala.collection.mutable.HashMap
import scala.collection.immutable.Vector
import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
    
class Node(id : String, nt : NodeType) extends Elem(id) {

    properties = Vector.fill(nt.numProperties())(null)

    def getProperties() = properties

    val sid = nt.addNode(this)
    
    def getId() = id

    def getSid() = sid
    
    def nodeType() = nt.getName()
    
    //def this(id : String,nt : NodeType, _properties : Vector[Complex | Rational | Real | StrO | StrNum | TimeO]) =
    //def this(id : String,nt : NodeType, _properties : Vector[Complex | Rational | Real | StrNum ]) =
    def this(id : String,nt : NodeType, _properties : Vector[Primitive]) = 
    {
        this(id,nt)
        for (p <- 0 until _properties.length){
            try {
                domainValidated(propertyType,propertyValue)
                
            }catch{
            }
            if ( t1 == t2 ) properties = properties :+ _properties(p)
            else {
                println(s"Warning : incompatible property and domain:")
                println(s"\t${t1},${t2}.")
                println(s"Node ${id} not added...")
                throw new Exception()
            }
        } // for
    } // this

    //def updateProperty(pName : String, property : Complex | Rational | Real | StrNum ) =
    def updateProperty(pName : String, property : Primitive) =
    {
        nt.updateNode(sid,pName,property) match
        {
            case Some(i)    =>  println(s"Node not updated: \n\t${i}")                                
            case None       =>  println(s"Node ${sid} property ${pName} updated to ${property}")
        }
    }
    
     // function to validate that the property values for a node all come from the right domains
	 def ValidatePropertiesInDomains(nt : NodeType, propertyValues: Vector[Primitive]) =
     {
	    val domains = nt.getDomains()
        
	    for ( i <- 0 until propertyValues.length) {
            var domnType = (domains(i).getClass).toString.replace("$","")   
            val pValType = (PropertyValues(i).getClass).toString.replace("class java.lang.","").replace("Integer","Int")
            if( !(domnType.equals(PropValueType) || pValueType.contains(domnType) || domnType.contains(pValType)) )  {
                //FINISH MESSAGE
                val message = s"""Type mismatch for ${}
                println(s"Type mismatch for ${PropNames(i)} (Required:${PropType.replace("class ","")} Found:${PropValueType})")
            } // if
        } // for
	    
	        
	        
	        
	            
	            MismatchCount	= MismatchCount+1
	        } // if
	    }//end for loop
	    return MismatchCount
     } //end CompareValuesWithTypes
    
}

object NodeTester extends App{
    val roadProperties = HashMap[String,String](
                            "name"->"String",
                            "type"->"String",
                            "district"->"int")

    val sensorProperties = HashMap[String,String](
                                "id"->"int",
                                "district"->"int")

    val road       = new NodeType("road",roadProperties)
    val sensor     = new NodeType("sensor",sensorProperties)

    val r1 = new Node("r1",road)
    val r2 = new Node("r2",road)
    val s1 = new Node("s1",sensor)
    val s2 = new Node("s2",sensor)

}