package kg

class InvalidPropertyException(message : String) extends Exception(message)
    
import scala.collection.mutable.HashMap    
import scala.math.max
trait ElementType(_name : String) {

    var numElements = 0 
    
    var elements = Vector[Element]()

    val schema = HashMap[String,PrimitiveType]("id"->Int)
 
    def apply(properties : Tuple2[String,Primitive]*) : Element =
    {
        val map = HashMap[String,Primitive]()
        for (property <- properties) map.update(property._1,property._2)
        val element = new Element(numElements)
        element.setProperties(map)
        elements = elements :+ element
        numElements = numElements+1
        element        
    }

    def apply(properties : HashMap[String,Primitive]) : Element =
    {
        val element = new Element(numElements)
        element.setProperties(properties)
        elements = elements :+ element
        element
    }
    
    def name = _name
    
    def getElements() : Vector[Element] = elements ++ Vector[Element]()
    
    def add(_elements : Element *) =
    {
        for (e <- _elements if (!elements.contains(e))) elements = elements :+ e
    }

    def add(_elements : Vector[Element]) =
    {
        for (e <- _elements if (!elements.contains(e))) elements = elements :+ e
    }
    
    def addSchema( pAndDs : Tuple2[String,PrimitiveType] *) =
    {
        for (pAndD <- pAndDs) schema.update(pAndD._1,pAndD._2)
    }

    def addSchema( schema : HashMap[String,PrimitiveType]) =
    {
        this.schema.addAll(schema)
    }

    def addProperty( property : Tuple2[String,PrimitiveType]) =
    {
        this.schema. += ((property._1,property._2))
    }
    
    def getSchema() = schema.clone()

    def contains(element : Element) = elements.contains(element)

    def empty() : ElementType =
    {
        object EmptyThing extends ElementType(_name)
        EmptyThing.addSchema(schema)
        return EmptyThing
        
    }
    def select  ( property  : String, 
                  p         : (Primitive) => Boolean) : ElementType =
    {
        
        object SelectThing
            extends ElementType(_name)

        //for (pd <- schema) SelectThing.addProperty(pd)
        SelectThing.addSchema(schema)
        schema.get(property) match{
            case Some(i)    =>
                {            
                    for (element <- elements) {
                        try{
                            if (p(element(property))) SelectThing.add(element)
                        }
                        catch{
                            case _ => 
                        }
                    }
                }
            case None       =>
                {
                    println(s"Invalid select property")
                    println(s"${property} not in schema for ${_name}")
                }
        } // match
        SelectThing
    }

    /*
     *  Return a projection of this ElementType projected
     *  onto a subset of its properties.
     */
    def project(properties : String*) : ElementType =
    {
        object ProjectThing extends ElementType(_name)

        ProjectThing.addSchema(schema.filter( (k,v) => {
            properties.contains(k)
        }))

        elements.foreach( e => {
            ProjectThing( e.getProperties().filter( (k,v) => {
                properties.contains(k)
            }))
        })

        ProjectThing
    }

    def union ( other : ElementType ) : ElementType =
    {
        
        object UnionThing extends ElementType(_name)
        UnionThing.addSchema(schema)
    
        if (schema == other.getSchema()){
            elements.foreach(element => UnionThing.add(element)) 
            other.getElements().foreach( element => {
                if (!UnionThing.contains(element)) UnionThing.add(element)
            })
        }
        else{
            println("Error in ElementType union.")
            println(s"This:\n${schema}")
            println(s"Not unnion compatible with other:\n${other.getSchema()}")       
        }
        return UnionThing
    }

    def intersect(other : ElementType) : ElementType =
    {
        object IntersectThing extends ElementType(_name)
        IntersectThing.addSchema(schema)
        if( schema == other.getSchema() ){
            elements.filter( element =>
                other.contains(element)
            ).foreach( element => 
                IntersectThing.add(element)  
            )
        } // if
        else{ 
            println(s"Incompatible ElementTypesfor intersect.")
            println(s"this: ${schema}")
            println(s"other: ${other.getSchema()}")
        }
        return IntersectThing
    }

    def minus(other : ElementType) : ElementType =
    {
        object MinusThing extends ElementType(_name)
        MinusThing.addSchema(schema)
        if( schema == other.getSchema() ){
            elements.filterNot( element =>
                other.contains(element)
            ).foreach( element => 
                MinusThing.add(element)  
            )
        } // if
        else{ 
            println(s"Incompatible ElementTypes for Intersect.")
            println(s"this: ${schema}")
            println(s"other: ${other.getSchema()}")
        }
        return MinusThing
        
    }

    def prettyPrintElements   ( name : String,
                                elements : Vector[HashMap[String,Primitive]],
                                properties : String*
                              ) : String =
    {
        val maxLength = properties.map(_.length).max

        val width =  properties.size * (maxLength +1) + 1
        val sep = "-" * (maxLength) + "+"
        val ssep = "+" + sep * properties.size
        val topBorder = ssep
        val botBorder = topBorder 
        
        var str = s"${name}:\n" + topBorder + "\n+"

        var args = List[Any]()
    
        for (property <- properties) {
            str = str + "%" + maxLength + "s+"
            args = args :+ property
        }

        str = str + "\n" + topBorder + "\n"
        for (element <- elements) {
            str = str + "+"
            for ( property <- properties ) {
                element.get(property) match{
                    case None       => {
                        str = str + "%"+maxLength+"s+"
                        args = args :+ ""
                    } // case None
                    case Some(i)    => {
                        args = args :+ i
                        schema.get(property) match{
                            //TODO MATCH THE CASES CORRECTLY
                            case _ => str = str + "%" + maxLength + "d+"
                        } // match
                    } // case Some(i)
                } // match
            } // for
            str += "\n" + ssep + "\n"
        } // for
        str.format(args:_*)
    }

    override def toString() =
    {
        val _schema = schema.toList
        val maxFieldWidths = Array.ofDim[Int](_schema.length)
        var maxValueLength = 0
        for ( property <- 0 until _schema.length ) {
            maxValueLength = {
            if (elements.length > 0) 
                elements.map( element => 
                    s"${element.getOrElse(_schema(property)._1,"")}".length
                ).max
            else 0
            }
            maxFieldWidths(property) = max(name.length,max(maxValueLength + 2,_schema(property)._1.length+2))
        }
        var str = "+"
        var border = "+"
        var value : Primitive = 0
        var lPad = 0
        var rPad = 0
        var pad = 0
        for ( i <- 0 until _schema.length){
            border = border + "-" * maxFieldWidths(i) + "+"
            pad = maxFieldWidths(i) - _schema(i)._1.length
            lPad = pad/2
            rPad = pad - lPad          
            str = str                       +
                  " " * lPad                +
                  _schema(i)._1.toUpperCase +   
                  " " * rPad                +
                  "+"
        }
        str = str + "\n" + border + "\n"
        for ( element <- elements) {
            for ( i <- 0 until _schema.length) {
                value = element.getOrElse(_schema(i)._1,"")
                pad = maxFieldWidths(i) - s"${value}".length
                lPad = pad/2
                rPad = pad-lPad
                str = str + s"+${" " * lPad}${value}${" " * rPad}"
            }
            str += "+\n"
            str += border + "\n"
        }
        pad = border.length - name.length - 2
        lPad = pad/2
        rPad = pad-lPad 
        return  s"+${"-" * (border.length-2) }+"               + "\n" +
                s"+${" "*lPad}${name.toUpperCase}${" "*rPad}+" + "\n" +
                border                                         + "\n" + 
                str 
    } // toString
}

class NodeType(name : String) extends ElementType(name){}
class EdgeType(name : String) extends ElementType(name){}

/*
 *  Tests the basic functionality of the NodeType and EdgeType classes:
 *      addSchema,
 *      add,
 *      apply, and
 *      print,
 *      select,
 *      project,
 *      union,
 *      intersect,
 *      minus
 *  Doesn't test schema restrictions...    
 */  
object ElementTypeTesterOne extends App{

    class Road(id : Int) extends Node(id){}
    object Road extends NodeType("Road"){}
    
    Road.addSchema(
        ("name",Int),
        ("type",Int),
        ("lanes",Int),
        ("dir",Int)
    )

    val r1 = Road(("name",2),("type",3),("lanes",2),("dir",1))
    val r2 = Road(("name",3),("type",2),("lanes",3),("dir",2))
    val r3 = Road(("name",4),("type",1),("lanes",4),("dir",1))
    val r4 = Road(("name",5),("type",1),("lanes",4),("dir",4))

    println(s"Road:\n${Road}")
    
    class Road2(id : Int) extends Node(id){}
    object Road2 extends NodeType("Road2"){}

    Road2.addSchema(Road.getSchema())

    val r5 = Road2(("name",2),("type",3),("lanes",2),("dir",1))
    
    println(s"Road2 before:\n${Road2}")
    Road2.add(Road.getElements())
    println(s"Road2 after:\n${Road2}")

    println("Selecting...")
    val Sel1 = Road.select("lanes",_==4)
    val Sel2 = Road2.select("dir",_==1)
    println(s"Sel1:\n${Sel1}")
    println(s"Sel2:\n${Sel2}")

    println("Projecting...")
    val Proj1 = Road.project("lanes","dir")
    val Proj2 = Sel1.project("type")
    val Proj3 = Road.project("invalid")
    println(s"Proj1:\n${Proj1}")
    println(s"Proj2:\n${Proj2}")
    println(s"Proj3:\n${Proj3}")

    println("Unioning...")
    val Sel3 = Road.select("lanes",_==4)
    val Sel4 = Road.select("lanes",_==2)
    val Sel5 = Road.select("lanes",_==3)
    val Uni1 = Sel3.union(Sel4).union(Sel5)
    println(s"Uni1: \n${Uni1}")

    println("Intersecting...")
    val Int1 = Sel1.intersect(Sel2)
    println(s"Int1:\n${Int1}")

    println("Minusing...")
    val Min1 = Road.minus(Sel1)
    println(s"Min1:\n${Min1}")

    val r6 = Road(("type",3),("lanes",2),("dir",1))
    val r7 = Road(("name",2),("lanes",2),("dir",1))

    println(s"r7: \n${r7}")
}

object ElementTypeTesterTwo extends App{

    class Road(id : Int) extends Node(id){}
    object Road extends NodeType("Road"){}
    
    Road.addSchema(
        ("name",Int),
        ("type",Int),
        ("lanes",Int),
        ("dir",Int)
    )

    val r1 = Road(("name",2),("type",3),("lanes",2),("dir",1))
    val r2 = Road(("name",3),("type",2),("lanes",3),("dir",2))
    val r3 = Road(("name",4),("type",1),("lanes",4),("dir",1))
    val r4 = Road(("name",5),("type",1),("lanes",4),("dir",4))

    println(s"Road:\n${Road}")
    
    for ( pv <- r1.iterator ) println(s"pv: ${pv}")
}