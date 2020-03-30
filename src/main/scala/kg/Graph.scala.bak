package kg

import scala.collection.mutable.HashMap

class Relation(_subj : Element, _pred : Element, _obj : Element){

    def subj = _subj.getLabel()
    def pred = _pred.getLabel()
    def obj  = _obj.getLabel()

    def subjNode = _subj
    def objNode  = _obj
    def edge     = _pred
    
    def apply(i : Int) : Option[String] =
    {
        i match {
            case 1 => subj
            case 2 => pred
            case 3 => obj
        }
    }
}

object Relation{
    def apply(n1 : Element, e: Element, n2 : Element) : Relation =
    {
        return new Relation(n1,e,n2)
    }
}


class KnowledgeGraph{
    val DEBUG = true
    //Useful constants for later
    private val SUBJ = 0
    private val PRED = 1
    private val OBJ = 2

    //The number of node types and edge types activated
    private var nNodeType = 0
    private var nEdgeType = 0

    /*  A vector of activated ElementTypes, and a map to find the
     *  place of a ElementType object in the vector
     */
    
    private val ntMap     = HashMap[NodeType,Int]()
    private var nodeTypes = Vector[NodeType]()

    private val etMap     = HashMap[EdgeType,Int]()
    private var edgeTypes = Vector[EdgeType]()

    /*
        Hashmaps to find relations in the graph.
        The key is the edge type.
        The value is a vector of relations:
            From vertex --> On Edge --> To Vertex
    */

    //Indexed by EdgeType label
    private val relationsByPred = HashMap[String, Vector[Relation]]()

    //Indexed by Subject NodeType
    private val relationsBySubj = HashMap[String, Vector[Relation]]()

    //Indexed by object NodeType
    private val relationsByObj = HashMap[String, Vector[Relation]]()

    private val relations = HashMap[Int,HashMap[String,Vector[Relation]]](
        SUBJ -> relationsBySubj,
        OBJ -> relationsByObj,
        PRED -> relationsByPred
    )
    /*
     *  The allowed schema for relations. Each tuple says that
     *  an edge of some EdgeType can lead from a node of some
     *  NodeType to a node of some (possibly other) NodeType.
     */
    private var schema = Vector[Tuple3[NodeType,EdgeType,NodeType]]()

    //Use these to enforce property uniqueness
    private var edgeProperties = Vector[Tuple2[String,PrimitiveType]]()
    private var nodeProperties = Vector[Tuple2[String,PrimitiveType]]()

    //Activate a Node/EdgeTypes on the graph
    def activateNodeType(nt : NodeType *) =
    {
        nt.foreach( x => {if (!nodeTypes.contains(x)){
            ntMap.update(x, nNodeType)
            nodeTypes = nodeTypes :+ x
            nNodeType = nNodeType + 1
        }})
    }

    def activateEdgeType(et : EdgeType *) =
    {
        if (!DEBUG){
            println("activating edge types...")
            et.foreach(x => println(s"$x"))
        }
        et.foreach( x => { if (!edgeTypes.contains(x)){
                etMap.update(x,nEdgeType)
                edgeTypes = edgeTypes :+ x
                nEdgeType = nEdgeType + 1
                if (!DEBUG) println(s"edgeType added: \n${x}")
        }})    
    }
    
    def allowRelation(rel : Tuple3[NodeType,EdgeType,NodeType]) =
        schema = schema :+ rel

    def addRelation(n1 : Element, e : Element, n2 : Element) : Unit =
        addRelation(Relation(n1,e,n2))

    def addRelation(relation : Relation) = 
    {
        //if the label exists use it to update the appropriate map
        relation.subj match{
            case Some(label)    => updateRelations(relation,label,SUBJ)
            case None           =>
        }
        relation.pred match{
            case Some(label)    => updateRelations(relation,label,PRED)
            case None           =>
        }
        relation.obj match{
            case Some(label)    => updateRelations(relation,label,OBJ)
            case None           =>
        }
    }

     
    def nodes(_nodeTypes : NodeType *) : KnowledgeGraph =
    {
        val nodeTypes = if (_nodeTypes.length == 0) this.nodetypes else _nodeTypes
        val g = new KnowledgeGraph()
        nodeTypes.foreach(nt => g.activateNodeType(nt))
        return g
    }

    def expandForward(_nodeTypes : NodeType *) =
    {
        val nodeTypes = if (_nodeTypes.length == 0) this.nodeTypes else _nodeTypes
        for(rel <- schema.filter( x =>{
            nodeTypes.contains(x._1) && nodeTypes.contains(x._3)}))
        {
            if (DEBUG) println("something")
            g.allowRelation(rel)
            g.activateEdgeType(rel._2)
            relationsByPred.get(rel._2.getName()) match
            {
                case None       => {}
                case Some(i)    => i.foreach(x => g.addRelation(x))
            }
        } // for 
        if (DEBUG) {
            println(s"leaving node(), edge types in g: ")
            for( x <- g.edgeTypes ) println(s"$x")
        } // if
        return g
    }

    /*
    def nodes(_nodeTypes : NodeType *) : KnowledgeGraph = 
    {
        val nodeTypes = if (_nodeTypes.length == 0) this.nodeTypes else _nodeTypes
        if (DEBUG) {
            if (_nodeTypes.length == 0) print("\tnull")
            println(s"\tnodeTypes in nodes():")
            for (nt <- nodeTypes) println(nt)
        }
        val g = new KnowledgeGraph()
        nodeTypes.foreach( nt => g.activateNodeType(nt) )
        if (DEBUG) {
            for( rel <- schema ) println(s"\trel: \n${rel}")
        }
        for(rel <- schema.filter( x =>{
            nodeTypes.contains(x._1) && nodeTypes.contains(x._3)}))
        {
            if (DEBUG) println("something")
            g.allowRelation(rel)
            g.activateEdgeType(rel._2)
            relationsByPred.get(rel._2.getName()) match
            {
                case None       => {}
                case Some(i)    => i.foreach(x => g.addRelation(x))
            }
        } // for 
        if (DEBUG) {
            println(s"leaving node(), edge types in g: ")
            for( x <- g.edgeTypes ) println(s"$x")
        } // if
        return g
    }
    */
    
    def edges() : KnowledgeGraph =
    {
        val g = new KnowledgeGraph()
        edgeTypes.foreach(et => g.activateEdgeType(et))
        return g
    }
    
    def edges(edgeTypes : EdgeType*) : KnowledgeGraph = 
    {
        val g = new KnowledgeGraph()
        edgeTypes.foreach(et => g.activateEdgeType(et))
        return g
    }

    def expandForward(edgeTypes : EdgeType*) : KnowledgeGraph =
    {
        if (DEBUG) {
            println("expanding forward with edge types...")
            for( et <- edgeTypes ) println(s"edgeType: ${et.getName()}")
            for( et <- this.edgeTypes) println(s"edgeType: ${et.getName()}")
        }
        val g = new KnowledgeGraph()
        val _edgeTypes = if (edgeTypes == null) edgeTypes else this.edgeTypes
        
        _edgeTypes.foreach( et =>
            {
                print(s"schema : ${schema}")
                schema.filter(
                    (subj,pred,obj) => pred.getName() == et.getName()
                ).foreach((subj,pred,obj) =>
                    {
                        if (DEBUG) {
                            println(s"""subj: ${subj.getName()},
                                |pred: ${pred.getName()},
                                |obj: ${obj.getName()}}".stripMargin""")
                        }

                        val subjType = g.getCorrespondingNodeType(subj) 
    	                g.activateNodeType(subjType)

                        val edgeType = g.getCorrespondingEdgeType(pred)
    	                g.activateEdgeType(edgeType)

    	                val objType  = g.getCorrespondingNodeType(obj)
    	                g.activateNodeType(objType)
    
                        g.allowRelation((subjType,edgeType,objType))

                        for (rel <- relations(PRED)(et.getName())){
                            subjType(rel.subjNode)
                            objType(rel.objNode)
                            edgeType(rel.edge)
                            g.addRelation(rel)  
                        } // for
    
    	            } // (subj,pred,obj) =>
    	        ) // foreach
            } // et =>
        ) // foreach
        return g 
    } // expandForward

    override def toString() : String =
    {
        var ret : String = nodeTypes.foldLeft("")( (x,y) => x + y.toString() + "\n")
        return ret
    }

    private def getCorrespondingEdgeType(edgeType : EdgeType) : EdgeType =
    {
        for ( et <- edgeTypes if (et.getName() == edgeType.getName()) ) return et
        object ET extends EdgeType(edgeType.getName())
        ET.addSchema(edgeType.getSchema())
        for (edge <- edgeType.getElements()) ET(edge)
        return ET
    }

    private def getCorrespondingNodeType(nodeType : NodeType) : NodeType =
    {

        for ( nt <- nodeTypes if (nt.getName() == nodeType.getName()) ) return nt
        object NT extends NodeType(nodeType.getName())
        NT.addSchema(nodeType.getSchema())
        for (edge <- nodeType.getElements()) NT(edge)
        return NT
    }

    private def updateRelations(relation : Relation,label : String, i : Int) =
    {
        val map = i match {
            case SUBJ => relationsBySubj
            case PRED => relationsByPred
            case OBJ  => relationsByObj    
        } // i match

    
        map.updateWith(label)({
            case None => Some(Vector[Relation](relation))
            case Some(i) => {
                val _i = i :+ relation
                Some(_i)  
            } // case Some(i)
        }) // map.updateWith...
    } // updateRelations
    
}
object KnowledgeGraphTester extends App{

    val g = new KnowledgeGraph()

    class Person extends Node{}
    object Person extends NodeType("Person"){}
    Person.addProperty("name",Int)

    val p1 = Person(("name",1))
    val p2 = Person(("name",2))
    val p3 = Person(("name",3))
    val p4 = Person(("name",4))
    val p5 = Person(("name",5))
    
    class Movie extends Node{}
    object Movie extends NodeType("Movie"){}
    Movie.addProperty("title",Int)

    val m1 = Movie(("title",1))
    val m2 = Movie(("title",2))

    class Acted_In extends Edge{}
    object Acted_In extends EdgeType("Acted_In"){}
    Acted_In.addProperty("role",Int)

    class Directed extends Edge{}
    object Directed extends EdgeType("Directed"){}
    
    g.activateNodeType(Person)
    g.activateNodeType(Movie)
    g.activateEdgeType(Acted_In)
    g.activateEdgeType(Directed)
    
    g.allowRelation(Person,Acted_In,Movie)
    g.allowRelation(Person,Directed,Movie)
    
    g.addRelation(p1,Acted_In(("role",1)),m1)
    g.addRelation(p2,Acted_In(("role",2)),m1)
    g.addRelation(p3,Directed()          ,m1)
    g.addRelation(p4,Acted_In(("role",3)),m1)

    g.addRelation(p2,Acted_In(("role",4)),m2)
    g.addRelation(p4,Acted_In(("role",5)),m2)
    g.addRelation(p5,Directed()          ,m2)

    println(s"getting all nodes via g.nodes() :\n${g.nodes()}")
    println(s"getting person nodes via g.nodes(Person)")
    println(s"${g.nodes(Person)}")
    println(s"getting movie nodes via g.nodes(Movie)")
    println(s"\n${g.nodes(Movie)}")

    println(s"g.edges()")
    println(s"${g.edges()}")
    println(s"g.nodes(Movie).expandForward(Acted_In)")
    println(s"${g.nodes(Movie).expandForward(Acted_In)}")
}
