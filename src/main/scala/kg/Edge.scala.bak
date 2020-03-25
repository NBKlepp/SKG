package kg

import scala.collection.mutable.HashMap

class Edge(from : Node, to : Node) extends Node{

    def getFrom() : Node = from.copy()
    def getTo() : Node = to.copy()

    override def copy() : Edge =
    {
        val edge : Edge = new Edge(from.copy(),to.copy())
        edge.properties.addAll(this.properties)
        return edge
    }
          
}