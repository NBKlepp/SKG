package kg

import scala.collection.mutable.HashMap
import scala.collection.immutable.Vector
    
class Relation(_subj : Element, _pred : Element, _obj : Element){

    val DEBUG = false
    
    def subj = _subj
    def pred = _pred
    def obj  = _obj

    override def toString() : String = 
    {
        if (DEBUG) {
            println(s"subj: \n${subj}")
            println(s"pred: \n${pred}")
            println(s"obj: \n${obj}")
        }
    
        s"(${subj.id})--[${pred.id}]-->(${obj.id})"
    }

}

object Relation{

    val DEBUG = false
    
    def apply(n1 : Element, e: Element, n2 : Element) : Relation =
    {
         if (DEBUG) {
            println(s"n1: ${n1.id}, e: ${e.id}, n2: ${n2.id}")
        }
        return new Relation(n1,e,n2)
    }
}

trait RelationPrinter{

    def relationTable(
        _relations   : Vector[Relation],
        nt1         : ElementType,
        et          : ElementType,
        nt2         : ElementType) : String =
    {
        val relations = _relations.filter( relation =>
            {
                nt1.contains(relation.subj) &&
                 et.contains(relation.pred) &&
                nt2.contains(relation.obj)
            }
        )
        var str = ""
        if (relations.length != 0) {
        	str = str + s"(${nt1.name})--[${et.name}]-->(${nt2.name})\n"
        	for (relation<-relations) str = str + s"${relation}\n"
        }
        return str
    }
}