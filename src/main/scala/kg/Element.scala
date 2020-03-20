package kg

import scala.collection.mutable.HashMap

abstract class Element {
    val properties = HashMap[String,Primitive]()
    def getProperties() : HashMap[String,Primitive] = properties.clone()
    def apply(property : String) : Primitive =
    {
        properties.get(property) match{
            case Some(i) => i
            case None   => throw new Exception("Property not found.")
        }
    }// apply

    def copy() : Element
    
    def setProperties(properties : HashMap[String,Primitive]) =
    {
        this.properties.clear()
        this.properties.addAll(properties)
    }
}