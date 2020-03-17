package kg  

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap

class InvalidProjectionProperty(message : String)
        extends Exception(message)

trait NodeAlgebraic extends NodeType{

    val DEBUG = true

    def project(name : String = null,
                properties : String*) : NodeAlgebraic =
    {
    
        class ProjectThing extends Node{}
        object ProjectThing
            extends NodeAlgebraic
            with NodeType(s"${
                    if (name!=null) name 
                    else s"${this.getName()}_PROJECTION"}")
        try{

            for (p <- properties if !schema.contains(p))
                throw new InvalidProjectionProperty(p)
        	for (pd <- schema if properties.contains(pd._1)){
        	    ProjectThing.addProperty(pd)
        	    ProjectThing.addPrimaryKey(pd._1)
        	} // for
        	
        	for (e <- elements.map
                (
                    x => x.filter(
                        (k,v) => properties.contains(k)
                    )
                )
            )
            ProjectThing(e)
        	return ProjectThing
        }catch{
            case ipp : InvalidProjectionProperty    =>
                {
                    var message = "Invalid projection property "
                    message = message + s"for NodetType ${getName()}\n"
                    message = message + s"${ipp.getMessage()}"
                    println(message)
                    return ProjectThing
                }
            case e : Throwable                      =>
                {
                    println("""Unexpected exception:
                            |${e.getMessage()}""".stripMargin)
                    return ProjectThing
                }
        } // catch
    }

    /*
     *  A method to project one NodeAlgebraic 
     *  object onto the schema of another
     */
    def _project(other : NodeAlgebraic, 
                 name : String = null) : NodeAlgebraic =
    {

        class ProjectThing extends Node{}
        object ProjectThing
            extends NodeAlgebraic
            with NodeType(s"${
                if (name!=null) name
                else s"${this.getName()}_PROJECTION"}")

        val otherSchema = other.getSchema()
        for( kv <- schema if otherSchema.contains(kv._1) ) 
            ProjectThing.addProperty(kv)
        for( property <- primaryKey )
            if ( other.getPrimaryKey().contains(property) && 
                 schema.get(property) == otherSchema.get(property) )
            ProjectThing.addPrimaryKey(property)

        val emptyPrimaryKey = ProjectThing.getPrimaryKey().size == 0
        for( property <- ProjectThing.getPrimaryKey() if emptyPrimaryKey ) 
            ProjectThing.addPrimaryKey(property) 

        return ProjectThing
    
    }
    
    def select  ( property    : String, 
                    p           : (Primitive) => Boolean,
                    name        : String = null
                ) : NodeAlgebraic =
    {
    
        class SelectThing extends Node{}
        object SelectThing
            extends NodeAlgebraic
            with NodeType(
                s"${
                    if (name!=null) name else s"${this.getName()}_SELECTION"}")
        try{
            validateSelect(property)
            for (pd <- schema) {
                SelectThing.addProperty(pd)
                if( primaryKey.contains(pd._1) ) 
                    SelectThing.addPrimaryKey(pd._1)
            }
            for (element <- elements.filter( x => p(x(property)))) 
                SelectThing(element)
            return SelectThing
        }catch{
            case e : Exception => 
                {
                    println(e.getMessage())
                    return SelectThing
                } // case e
        } // catch  
    }
        
    def union   (   other : NodeAlgebraic,
                    name : String = null ) : NodeAlgebraic =
    {
        if (DEBUG) {
            println(s"From union...\nthis:\n${this}\nother:\n${other}")
        }
        class UnionThing extends Node{}
        object UnionThing
            extends NodeAlgebraic
            with NodeType(s"${
                if (name!=null) name
                else s"${
                    this.getName()
                }_UNION_${
                    other.getName()
                }"
            }")

        if (schema == other.getSchema()){        	
        	for (kv <- schema){
        	    UnionThing.addProperty(kv._1,kv._2)
        	    if (primaryKey.contains(kv._1)) UnionThing.addPrimaryKey(kv._1)
        	} // for
        	
        	val uniqueified            = uniqueify(other)
        	val uniqueifiedSchema      = uniqueified._1
        	val uniqueifiedPrimaryKeys = uniqueified._2
        	
        	for (kv <- uniqueifiedSchema) {
        	    UnionThing.addProperty(kv._1,kv._2)
        	    if (uniqueifiedPrimaryKeys.contains(kv._1)) 
                    UnionThing.addPrimaryKey(kv._1)
        	} // for
        	
        	if (DEBUG) {
        	    println(s"Union primary key : ${UnionThing.getPrimaryKey()}")
        	}
        	
        	val newNodes = elements.foldLeft(other.getNodes())( 
                (x,y) => {if (!x.contains(y)) x :+ y.clone() else x} )
        	val utPrimaryKey = UnionThing.getPrimaryKey()
        	for (element <- newNodes) {
        	    for (property <- utPrimaryKey) {
        	        element.get(property) match{
        	            case None    => element.update(property,null)
        	            case Some(i) =>
        	        } // match
        	    } // for
        	    UnionThing(element)    
        	} // for 
        	if (DEBUG) {
        	    println(s"From union...\nthis:\n${this}\nother:\n${other}")
        	}
        	return UnionThing
        }else{
            println("Error in NodeAlgebraic union.")
            println(s"This:\n${schema}")
            println(s"Not unnion compatible with other:\n${other.getSchema()}")
            return UnionThing
        }
    }

    private def uniqueify(other : NodeAlgebraic) : 
        Tuple2[HashMap[String,PrimitiveType],Vector[String]] =
    {
        val uniqueifiedSchema = HashMap[String,PrimitiveType]()
        var uniqueifiedPrimaryKey = Vector[String]()
        val otherPrimaryKey = other.getPrimaryKey()
        for ( kv <- other.getSchema() ) {
            schema.get(kv._1) match {
                case Some(i)    =>     
                    uniqueifiedSchema.update(s"${
                        kv._1}_${other.getName()
                    }",kv._2)
                case None       =>  
                    uniqueifiedSchema.update(kv._1,kv._2)
            } // match
            if (otherPrimaryKey.contains(kv._1)) 
                uniqueifiedPrimaryKey = uniqueifiedPrimaryKey :+ kv._1
        } // for
        return Tuple2(uniqueifiedSchema,uniqueifiedPrimaryKey)
    }

    /*  QUESTION : Should we be looking structurally at the intersect?
     *      i.e. -  if one schema is a subset of the other, should we
     *              return the members of that subset which are present
     *              subset-wise in the superset nodes?
     */
    def intersect(other : NodeAlgebraic,name : String = null) : NodeAlgebraic = 
    {
        class IntersectThing extends Node{}
        object IntersectThing
            extends NodeAlgebraic
            with NodeType(s"${
                if (name!=null) name
                else s"${this.getName()}_INTERSECT_${other.getName()}"}")
        if(
                schema == other.getSchema() && 
                primaryKey.toSet == other.getPrimaryKey().toSet)
            {
                for (kv <- schema) {
                    IntersectThing.addProperty(kv._1,kv._2)
                    if (primaryKey.contains(kv._1)) 
                        IntersectThing.addPrimaryKey(kv._1)
                } // for 
                val those = other.getNodes()
                val newNodes = elements.foldLeft( 
                    Vector[HashMap[String,Primitive]]() 
                )( 
                    (x,y) => {if(those.contains(y)) x :+ y.clone() else x} 
                )
                for (element <- newNodes) IntersectThing(element)
        } // if
        return IntersectThing
    }

    /*
     *  A structural intersection
     */
    def _intersect(other : NodeAlgebraic,name : String = null) : NodeAlgebraic =
    {
        class IntermediateThing1 extends Node{}
        val IntermediateThing1 = this._project(other,other.getName())

        class IntermediateThing2 extends Node{}
        val IntermediateThing2 = other._project(IntermediateThing1,getName())
    
        class IntersectThing extends Node{}
        val IntersectThing = IntermediateThing1.intersect(IntermediateThing2)        
        
        return IntersectThing
    
        /*    
        for (kv <- IntermediateThing1.getSchema()) 
            IntersectThing.addProperty(kv._1,kv._2)

        val it1Schema = IntermediateThing1.getSchema()
        val newPrimaryKey = primaryKey.filter(
            it1Schema.keySet.contains(_) && it1Schema.get(_) == schema.get(_)
        )
        val emptyNewPrimaryKey = newPrimarykey.size == 0

        primaryKey.foreach({
            if      (emptyNewPrimaryKey)     IntermediateThing1.addPrimaryKey(_)
            else if (primaryKey.contains(_)) IntermediateThing1.addPrimaryKey(_)
        }) // foreach
    
        val those = intermediateThing2.getNodes()
        val newNodes = 
            intermediateThing1.
            getNodes().
            foldLeft(
                Vector[HashMap[String,Primitive]]() 
            )( 
                (x,y) => {if(those.contains(y)) x :+ y else x} 
            )
        for (element <- newNodes) IntersectThing(element)
        
        IntersectThing
        */
    }

    // SAME QUESTION AS ABOVE 
    def minus(other : NodeAlgebraic, name : String = null) =
    {
        if (DEBUG) {
            println(s"from minus... \nthis: \n${this}\nother: \n${other}")
        }
        class MinusThing extends Node{}
        object MinusThing
            extends NodeAlgebraic
            with NodeType(s"${
                if (name!=null) name
                else s"${this.getName()}_MINUS_${other.getName()}"}")
        
        if (
            schema == other.getSchema() && 
            (primaryKey.toSet == other.getPrimaryKey().toSet )
        ){
            for (kv <- schema) {
                MinusThing.addProperty(kv._1,kv._2)
                if (primaryKey.contains(kv._1) ) MinusThing.addPrimaryKey(kv._1)
            } // for 
            val those = other.getNodes()
            val newNodes = elements.foldLeft( 
                Vector[HashMap[String,Primitive]]() 
            )( 
                (x,y) => if (!those.contains(y)) x :+ y else x
            )
            for (element <- newNodes) MinusThing(element)
        } // if
        MinusThing
    }

    def _minus(other : NodeAlgebraic, name : String = null) : NodeAlgebraic = 
    {

        class IntermediateThing1 extends Node{}
        val IntermediateThing1 = this._project(other,"it1")

        class IntermediateThing2 extends Node{}
        val IntermediateThing2 = other._project(IntermediateThing1,"it2")

        class MinusThing extends Node{}
        val MinusThing = IntermediateThing1.minus(
            IntermediateThing2,s"{getName()}_MINUS_${other.getName()}")   

        return MinusThing
    }

    private def validateProjectionProperties(properties : String *) =
    {
        for ( p <- properties if !schema.keySet.contains(p) ) {
            var message = s"""Invalid property to project these elements onto.
                            |$p not in ${schema.keys}.""".stripMargin
            throw new Exception(message)
        }
    }

    private def validateSelect(property : String) =
    {
        if (!schema.keySet.contains(property)) {
            var message = s"""NodeAlgebraic select error.
                |Invalid property for element type """.stripMargin
            message = message + s"${getName()}: ${property}"
            throw new Exception(message)
        }
    }
}