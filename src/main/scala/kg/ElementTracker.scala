package kg

class InvalidPropertyException(message : String) extends Exception(message)
    
import scala.collection.mutable.HashMap    
    
trait ElementTracker[T <: Element] {

    var elements = Vector[T]()
    val schema = HashMap[String,PrimitiveType]()

    def add(element : T) = {elements = elements :+ element}
    
    def validate(properties : HashMap[String,Primitive]) =
    {
        //validatePrimaryKey(properties)
        for ( pv <- properties ) {

            val property    = pv._1
            val value       = pv._2
    
            schema.get(property) match {
    
                case Some(i)    =>  validatePropertyValueDomain(i,property,value) 
                case None       =>  {
                    var message = s"Property ${property} not in schema."
                	throw new InvalidPropertyException(message)
                } // case None
            } // match
        } // for
    } // valiDomains

    
    
    /*
     * Get the number of properties for this ElementTracker object
     *
     */
    def schemaSize = schema.size
    
    /*
     * Get the number of elements for this ElementTracker object.
     *
     */
    def elementsSize = elements.size

    /*
     * Add a property to this element type
     *
     */
    def addProperty(property : String, domain : PrimitiveType) = schema.update(property,domain)
    

    /*
     * Add a property to this element type
     *
     */    
    def addProperty( pd : Tuple2[String,PrimitiveType]) : Unit = addProperty(pd._1,pd._2)
    

    /*
     *  Add the contents of one schema to the schema of this ElementTracker
     *
     */    
    def addSchema( schema : HashMap[String,PrimitiveType] ) = this.schema.addAll(schema) 
    

    /*
    def addPrimaryKey( property : String ) =
    {
        schema.get(property) match {
            case Some(i)     => primaryKey = primaryKey :+ property
            case None        => 
                println(s"""WARNING: {property} not a property for element type ${name}
                        |{property} not added as primary key property.""".stripMargin)
        }
    }
    */

    /*
     *  Get a copy of the schema for this ElementTracker
     *
     */
    def getSchema() = schema.clone()

    //def getPrimaryKey() = primaryKey ++ Vector[String]()

    /*
     *  Get a copy of the vector of element for this ElementTracker
     *
     */    
    def getElements() : Vector[T] = elements ++ Vector[T]()

    /*
     *  Get a String representation of this ElementTracker
     *
     */    
    override def toString() =
    {
        var str = s"|"
        var args = List[Any]()
        val keys = schema.keySet
    
        val maxLength = keys.map(_.length).foldLeft( 0 )( 
            (x,y) => {if (x>=y) x else y} )

        for (property <- keys) {
            str = str + "%" + maxLength + "s|"
            args = args :+ property
        }

        str = str + "\n"    
        for (element <- elements.map(x=>x.getProperties())) {
            str = str + "|"
            for ( property <- keys ) {
                element.get(property) match{
                    case None       => {
                        str = str + "%"+maxLength+"s|"
                        args = args :+ ""
                    } // case None
                    case Some(i)    => {
                        args = args :+ i
                        schema.get(property) match{
                            //TODO MATCH THE CASES CORRECTLY
                            case _ => str = str + "%" + maxLength + "d|"
                        } // match
                    } // case Some(i)
                } // match
            } // for
            str += "\n"
        } // for
        str.format(args:_*)
     // toString
    }
    def validatePropertyValueDomain(i : PrimitiveType,
                                            property: String,
                                            value : Primitive) =
    {
        val expected = (i.getClass).
                          toString.
                          replace("$","").
                          replace("class scala.","")
        val passed = (value.getClass).
                          toString.
                          replace("class java.lang.","").
                          replace("Integer","Int")
        if ( expected != passed ){
            var message = s"""invalid data type for property ${property}.
                            |Value ${value} has Type ${passed}.  
                            |Type {expected} exptected.""".stripMargin
            throw new Exception(message)
                                
        }
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
    private def validatePrimaryKey(element : HashMap[String,Primitive]) =
    {
        if ( primaryKey.size == 0 ) 
            throw new Exception("No primary key defined for element type {name}.")
        for ( k <- primaryKey if !element.keySet.contains(k) ) {
            var message = s"Missing primary key value for ${k} "
            message = message + s"property of element type ${name}"
            throw new Exception(message)
        }
    }
    */
}