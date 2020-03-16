package kg
import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap

trait GraphAlgebra extends ElementTracker{
    
    def select(property : String, p : (Primitive) => Boolean) =
    {
        class NewThing extends NodeType{}
        object NewThing extends GraphAlgebra
        for (kv <- schema) NewThing.addProperty(kv._1,kv._2)
        val unionElements = elements.filter( x => p(x(property)))
        for (element <- unionElements) NewThing(element)
        NewThing
    }
    
    
    def project(properties : String*) =
    {
        class ProjectThing extends NodeType{}
        object ProjectThing extends GraphAlgebra
        for (kv <- schema if properties.contains(kv._1)) ProjectThing.addProperty(kv._1,kv._2)
        val newElements = elements.map( x => x.filter( (k,v) => properties.contains(k)))
        for (element <- newElements) ProjectThing(element)
        ProjectThing
    }

    def union[T <: ElementTracker](nodeType : T) =
    {
        class NewThing extends NodeType{}
        object NewThing extends GraphAlgebra
        for (kv <- schema              ) NewThing.addProperty(kv._1,kv._2)
        for (kv <- nodeType.getSchema()) NewThing.addProperty(kv._1,kv._2)
        val newElements = elements.foldLeft( nodeType.getElements() )( (x,y) => {if (!x.contains(y)) x :+ y else x} )
        for (element <- newElements) NewThing(element)
        NewThing
    }

    def intersect[T <: ElementTracker](nodeType : T) =
    {
        class NewThing extends NodeType{}
        object NewThing extends GraphAlgebra
        if (schema == nodeType.getSchema()){
            for (kv <- schema              ) NewThing.addProperty(kv._1,kv._2)
            val those = nodeType.getElements()
            val newElements = elements.foldLeft( Vector[HashMap[String,Primitive]]() )( (x,y) => {if(those.contains(y)) x :+ y else x} )
            for (element <- newElements) NewThing(element)
        } // if
        NewThing
    }

    def minus[T <: ElementTracker](nodeType : T) =
    {
        class NewThing extends NodeType{}
        object NewThing extends GraphAlgebra
        if (schema == nodeType.getSchema()){
            for (kv <- schema              ) NewThing.addProperty(kv._1,kv._2)
            val those = nodeType.getElements()
            val newElements = elements.foldLeft( Vector[HashMap[String,Primitive]]() )( (x,y) => if (!those.contains(y)) x :+ y else x)
            for (element <- newElements) NewThing(element)
        } // if
        NewThing
    }
        
    
}