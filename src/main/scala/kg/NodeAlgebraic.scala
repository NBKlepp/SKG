package kg  

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap

class InvalidProjectionProperty(message : String)
        extends Exception(message)

trait NodeAlgebraic extends NodeType{

    val DEBUG = true

    def project(properties : String*) : NodeAlgebraic =
    {
    
        class ProjectThing extends Node{}
        object ProjectThing
            extends NodeAlgebraic
            with NodeType(s"${this.getName()}_PROJECTION")

        for (p <- properties if !schema.contains(p)) 
            println("Invalid projection property for ${getName()}: ${p}")
        
        for (pd <- schema if properties.contains(pd._1))
            ProjectThing.addProperty(pd)
        
        for (element <- elements.map( x => x.getProperties().filter(
            (k,v) => properties.contains(k)))) ProjectThing(element)

        return ProjectThing

    }

    /*
     *  A method to project one NodeAlgebraic 
     *  object onto the schema of another
     */
    def _project(other : NodeAlgebraic) : NodeAlgebraic =
    {

        class ProjectThing extends Node{}
        object ProjectThing
            extends NodeAlgebraic
            with NodeType(s"${this.getName()}_PROJECTION")

        val otherSchema = other.getSchema()
        for( kv <- schema if otherSchema.contains(kv._1) ) 
            ProjectThing.addProperty(kv)

        for (e <- elements.map( x => x.getProperties().filter(
            (k,v) => otherSchema.keySet.contains(k)))) ProjectThing(e)
    
        return ProjectThing
    
    }
    
    def select  ( property  : String, 
                  p         : (Primitive) => Boolean) : NodeAlgebraic =
    {
        class SelectThing extends Node{}
        object SelectThing
            extends NodeAlgebraic
            with NodeType(s"${this.getName()}_SELECTION")
        schema.get(property) match{
            case Some(i)    =>
                {
                    for (pd <- schema) SelectThing.addProperty(pd)
                    for (element <- elements.filter( x => p(x(property)))) 
                    SelectThing.add(element)
                }
            case None       => println(s"Invalid select property : ${property}")
        } // match
        SelectThing
    }
        
    def union ( other : NodeAlgebraic ) : NodeAlgebraic =
    {
        if (DEBUG) println(s"From union...\nthis:\n${this}\nother:\n${other}")
        
        class UnionThing extends Node{}
        object UnionThing
            extends NodeAlgebraic
            with NodeType(s"${getName()}_UNION_${other.getName()}")
        
        if (schema == other.getSchema()){        	

        	for (kv <- schema) UnionThing.addProperty(kv._1,kv._2)

        	val newElements = elements.foldLeft(other.getElements())( 
                (x,y) => {if (!x.contains(y)) x :+ y else x} )

        	for (element <- newElements) UnionThing.add(element)    

        	if (DEBUG) {
        	    println(s"From union...\nthis:\n${this}\nother:\n${other}")
        	}
        }else{
            println("Error in NodeAlgebraic union.")
            println(s"This:\n${schema}")
            println(s"Not unnion compatible with other:\n${other.getSchema()}")
            
        }
        return UnionThing
    }

    /*  QUESTION : Should we be looking structurally at the intersect?
     *      i.e. -  if one schema is a subset of the other, should we
     *              return the members of that subset which are present
     *              subset-wise in the superset nodes?
     */
    def intersect(other : NodeAlgebraic) : NodeAlgebraic = 
    {
        class IntersectThing extends Node{}
        object IntersectThing
            extends NodeAlgebraic
            with NodeType(s"${this.getName()}_INTERSECT_${other.getName()}")
        if( schema == other.getSchema() ){
            for (kv <- schema) IntersectThing.addProperty(kv._1,kv._2)
            
            for (element <- elements if other.getElements().contains(element)) 
                IntersectThing.add(element)     
            
        } // if
        else{ 
            println(s"Incompatible NodeTrackers for intersect.")
            println(s"this: ${schema}")
            println(s"other: ${other.getSchema()}")
        }
        return IntersectThing
    }

    /*
     *  A structural intersection
     */
    def _intersect(other : NodeAlgebraic,name : String = null) : NodeAlgebraic =
    {
        class IntermediateThing1 extends Node{}
        val IntermediateThing1 = this._project(other)

        class IntermediateThing2 extends Node{}
        val IntermediateThing2 = other._project(IntermediateThing1)
    
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
        val newElements = 
            intermediateThing1.
            getElements().
            foldLeft(
                Vector[HashMap[String,Primitive]]() 
            )( 
                (x,y) => {if(those.contains(y)) x :+ y else x} 
            )
        for (element <- newElements) IntersectThing(element)
        
        IntersectThing
        */
    }

    // SAME QUESTION AS ABOVE 
    def minus(other : NodeAlgebraic) : NodeAlgebraic =
    {
        if (DEBUG) {
            println(s"from minus... \nthis: \n${this}\nother: \n${other}")
        }
        class MinusThing extends Node{}
        object MinusThing
            extends NodeAlgebraic
            with NodeType(s"${this.getName()}_MINUS_${other.getName()}")
        
        if ( schema == other.getSchema() ){
            for (kv <- schema) {
                MinusThing.addProperty(kv._1,kv._2)
                
            } // for

            for( element <- elements if !(other.getElements().contains(element)) ) MinusThing.add(element)  
        } // if
        else{
            println("Incompatible node-types for minus oepration.")
            println(s"This: ${this}")
            println(s"Other: ${other}")
        }
        MinusThing
    }

    def _minus(other : NodeAlgebraic, name : String = null) : NodeAlgebraic = 
    {

        class IntermediateThing1 extends Node{}
        val IntermediateThing1 = this._project(other)

        class IntermediateThing2 extends Node{}
        val IntermediateThing2 = other._project(IntermediateThing1)

        class MinusThing extends Node{}
        val MinusThing = IntermediateThing1.minus(IntermediateThing2)   

        return MinusThing
    }
}