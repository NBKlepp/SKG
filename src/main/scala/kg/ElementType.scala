package kg

class InvalidPropertyException(message : String) extends Exception(message)
    
import scala.collection.mutable.HashMap    
    
trait ElementType(_name : String) {

    var name = _name
    var elements = Vector[Element]()
    val schema = HashMap[String,PrimitiveType]()

    def reName(name : String) = {this.name = name}
    def getName() : String = name

    private def add(element : Element) =
    {
        if (!elements.contains(element) ) elements = elements :+ element
    }

    def apply(properties : HashMap[String,Primitive]) : Element =
    {
        try{
            val element = new Element()
            validate(properties)
            element.setProperties(properties)
            element.setLabel(name) 
            elements = elements :+ element
            element
        }catch{
            case e  : Exception =>
                {
                    println(s"WARNING: Invalid element for ElementType ${name}.")
                    println(s"${e.getMessage()}")
                    println("Element Not Added.")
                    throw new Exception()
                } // case e
        } // catch
    } // apply

    def apply(element : Element) = //: Tuple2[ElementType,Element] = 
    {
        try{
            validate(element.getProperties())
            elements = elements :+ element
            //return Tuple2[ElementType,Element](this,element)
        }catch{
            case e  : Exception =>
                {
                    println(s"WARNING: Invalid element for ElementType ${name}.")
                    println(s"${e.getMessage()}")
                    println("Element Not Added.")
                    throw new Exception()
                } // case e
        } // catch
    }

    def apply(properties : Tuple2[String,Primitive]*) : Element =
    {
        val map = HashMap[String,Primitive]()
        for (property <- properties) map.update(property._1,property._2)
        apply(map)
    }

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

    /*
     *  Tells whether or not this ElementType contains a particular element.
     */
    def contains(element : Element) = this.elements.contains(element)
    
    //def getPrimaryKey() = primaryKey ++ Vector[String]()

    /*
     *  Get a copy of the vector of element for this ElementTracker
     *
     */    
    def getElements() : Vector[Element] = elements ++ Vector[Element]()


    def prettyPrintElements   ( name : String,
                                elements : Vector[HashMap[String,Primitive]],
                                properties : String*
                              ) : String =
    {
        val maxLength = properties.map(_.length).foldLeft( 0 )( 
            (x,y) => {if (x>=y) x else y} )

        val width =  properties.size * (maxLength +1) + 1
        val topBorder = s"+${"-" * (width-2)}+"
        val botBorder = s"+${"-" * (width-2)}+" //"\u203E" * width 
        
        var str = s"${name}:\n" + topBorder + "\n|"

        var args = List[Any]()
    
        for (property <- properties) {
            str = str + "%" + maxLength + "s|"
            args = args :+ property
        }

        str = str + "\n"
        for (element <- elements) {
            str = str + "|"
            for ( property <- properties ) {
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
        str = str + botBorder
        str.format(args:_*)
    }
     
    /*
     *  Get a String representation of this ElementTracker
     *
     */    
    override def toString() =
    {
        /*
        val keys = schema.keySet
        val maxLength = keys.map(_.length).foldLeft( 0 )( 
            (x,y) => {if (x>=y) x else y} )
        println("+")
        val width =  schema.size * (maxLength +1) + 1
        val topBorder = s"+${"-" * (width-2)}+"
        val botBorder = s"+${"-" * (width-2)}+"//"\u203E" * width 
        
        var str = s"${name}:\n" + topBorder + "\n|"

        var args = List[Any]()
    
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
        str = str + botBorder
        str.format(args:_*)
        */
        prettyPrintElements(name,elements.map(_.getProperties()),schema.keySet.toArray:_*)
    } // toString

    private def validate(properties : HashMap[String,Primitive]) =
    {
    
        if (properties != null) {
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
        } // if
    } // valiDomains
    
    private def validatePropertyValueDomain(i       : PrimitiveType,
                                            property: String       ,
                                            value   : Primitive     ) =
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
                            |Type ${expected} exptected.""".stripMargin
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

    /*
     *  Return a view of this ElementType projected
     *  onto a subset of its properties.
     */
    def project(properties : String*) : String =
    {
        for (p <- properties if !schema.contains(p)) 
            println(s"Invalid projection property for ${getName()}: ${p}")

        val validProperties = properties.filter(schema.contains(_))
        val newElements = elements.map( _.getProperties().filter(
            x => validProperties.contains(x._1)
        ))

        var pName = "["
        pName = validProperties.foldLeft(pName)(
            (x,y) => x + y + "," )
        pName = pName.substring(0,pName.length-1) + "]"
        prettyPrintElements(s"${name}${pName}",newElements,validProperties:_*)
    }

    def select  ( property  : String, 
                  p         : (Primitive) => Boolean) : ElementType =
    {
        
        object SelectThing
            extends ElementType(name)

        //for (pd <- schema) SelectThing.addProperty(pd)
        SelectThing.addSchema(schema)
        schema.get(property) match{
            case Some(i)    =>
                {            
                    for (element <- elements) {
                        try{
                            if (p(element(property))) SelectThing(element)
                        }
                        catch{
                            case _ => 
                        }
                    }
                }
            case None       =>
                {
                    println(s"Invalid select property")
                    println(s"${property} not in schema for $name")
                }
        } // match
        SelectThing
    }
    
    def union ( other : ElementType ) : ElementType =
    {
        
        object UnionThing
            extends ElementType(name)
            //extends ElementType(s"${getName()}_UNION_${other.getName()}")

        UnionThing.addSchema(schema)   
        if (schema == other.getSchema()){        	
            for( element <- elements ) UnionThing(element)
            for( element <- other.getElements() if !UnionThing.contains(element))
                UnionThing(element)
        }else{
            println("Error in ElementType union.")
            println(s"This:\n${schema}")
            println(s"Not unnion compatible with other:\n${other.getSchema()}")       
        }
        return UnionThing
    }

    def unionAll ( other : ElementType ) : ElementType =
    {
        
        object UnionThing
            extends ElementType(name)
            //extends ElementType(s"${getName()}_UNION_${other.getName()}")

        UnionThing.addSchema(schema)
        if (schema == other.getSchema()){        	
            for( element <- elements ) UnionThing(element)
            for( element <- other.getElements() ) UnionThing(element)
        }else{
            println("Error in NodeAlgebraic union.")
            println(s"This:\n${schema}")
            println(s"Not unnion compatible with other:\n${other.getSchema()}")
            
        }
        return UnionThing
    }

    def intersect(other : ElementType) : ElementType =
    {
        object IntersectThing
            extends ElementType(name)
            //extends ElementType(s"${name}_INTERSECT_${other.getName()}")
    
        if( schema == other.getSchema() ){
            IntersectThing.addSchema(schema)
           
            for (element <- elements if other.contains(element)) 
                IntersectThing(element)     
            
        } // if
        else{ 
            println(s"Incompatible NodeTrackers for intersect.")
            println(s"this: ${schema}")
            println(s"other: ${other.getSchema()}")
        }
        return IntersectThing
    }

    // SAME QUESTION AS ABOVE 
    def minus(other : ElementType) : ElementType =
    {
        object MinusThing
            extends ElementType(name)
            //extends ElementType(s"${name}_MINUS${other.getName()}")
        
        if ( schema == other.getSchema() ){
            MinusThing.addSchema(schema)
            for( element <- elements if !(other.contains(element)) ) MinusThing(element)  
        } // if
        else{
            println("Incompatible node-types for minus oepration.")
            println(s"This: ${this}")
            println(s"Other: ${other}")
        }
        MinusThing
    }   
}

class NodeType(name : String) extends ElementType(name){
    /*
    def shortCopy() : NodeType =
    {
        object CopyThing extends NodeType(name)
        CopyThing.addSchema(schema)
        return CopyThing
    }
    */
}
class EdgeType(name : String) extends ElementType(name){
    /*
    def shortCopy() : EdgeType =
    {
        object CopyThing extends EdgeType(name)
        CopyThing.addSchema(schema)
        return CopyThing
    }
    */
}

object ElementTypeTesterOne extends App{

    println("Defining Nodes & NodeTypes and Edgeds & EdgeTypes...")
    
    class Road extends Node{}
    object Road extends NodeType("Road"){}

    Road.addProperty("name",Int)
    Road.addProperty("dir",Int)
    Road.addProperty("lanes",Int)
    Road.addProperty("type",Int)
    
    class  Intersection extends Edge{}
    object Intersection extends EdgeType("Intersection"){}

    Intersection.addProperty("stops",Int)
    Intersection.addProperty("absPM1",Int)
    Intersection.addProperty("absPM2",Int)
    
    println("Instantiating roads and intersections...")
    println("Creating roads...")

    try{
        val r1 = Road(HashMap[String,Primitive](
            "name"->1.0,"type"->4,"lanes"->1,"dir"->1))
    }catch{
        case e : Throwable => println("Invalid element throws exception: Pass")
    }

    val r2 = Road(("name",2),("type",3),("lanes",2),("dir",1))
    val r3 = Road(("name",3),("type",2),("lanes",3),("dir",2))
    val r4 = Road(("name",4),("type",1),("lanes",4),("dir",1))
    val r5 = Road(("name",5),("type",1),("lanes",4),("dir",4))
    val r6 = Road()

    println("Creating intersections...")

    val i23 = Intersection(("stops",2),("absPM1",10),("absPM2",5 ))
    val i24 = Intersection(("stops",4),("absPM1",5) ,("absPM2",5 ))
    
    val i32 = Intersection(("stops",4),("absPM1",10),("absPM2",5 ))
    val i35 = Intersection(("stops",0),("absPM1",10),("absPM2",10))

    val i42 = Intersection(("stops",2),("absPM1",5) ,("absPM2",5 ))
    val i45 = Intersection(("stops",0),("absPM1",10),("absPM2",5 ))

    val i54 = Intersection(("stops",0),("absPM1",5) ,("absPM2",10))
    val i53 = Intersection(("stops",0),("absPM1",10),("absPM2",10))

    println(s"Roads:\n${Road}\n\nIntersections:\n${Intersection}")

    println(s"Road schema: ${Road.getSchema()}")

    println("Changing names...")
    Road.reName("Street")
    Intersection.reName("Intersects")
    println(s"Roads...\n${Road}\n\nIntersections...\n${Intersection}")

    println("Projecting...")
    println(Road.project("name","lanes","dir","typp"))
    println(Intersection.project("stops","absPM1"))

    println("Selecting...")
    val Sel1 = Road.select("lanes",_==4)
    val Sel2 = Road.select("dir",_==1)
    val Sel3 = Road.select("stops",_==2)
    val Sel4 = Intersection.select("stops",_==2)

    Sel1.reName("Road[lanes==4]")
    Sel2.reName("Road[type==1]")
    Sel3.reName("Road[stops==2]")
    Sel4.reName("Intersection[stops==2]")

    println(s"Sel1: \n${Sel1}")
    println(s"Sel2: \n${Sel2}")
    println(s"Sel3: \n${Sel3}")
    println(s"Sel4: \n${Sel4}")

    println("Union tests...")
    val U1 = Sel1.union(Sel2)
    val U2 = Sel1.unionAll(Sel2)
    val U3 = Sel1.union(Sel4)

    U1.reName("Sel1.union(Sel2)")
    U2.reName("Sel1.unionAll(Sel2)")
    U3.reName("Sel1.union(Sel4)")

    println(U1)
    println(U2)
    println(U3)

    println("Testing intersection...")
    val I1 = Sel1.intersect(Sel2)
    I1.reName("Sel1.intersect(Sel2)")
    println(s"I1...\n${I1}")

    println("Testing minus...")
    val M1 = Sel1.minus(Sel2)
    M1.reName("Sel1.minus(Sel2)")
    println(s"M1...\n${M1}")   
}