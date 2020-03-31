package kg

import scala.collection.mutable.HashMap
import scala.collection.immutable.Vector

class Graph{

    val DEBUG = false

    val NODE = 1
    val EDGE = 2

    val SUBJ = 1
    val PRED = 2
    val OBJ = 3

    val FORWARD = 1
    val BACKWARD = 2
    
    var nodeTypes = Vector[ElementType]()
    var edgeTypes = Vector[ElementType]()

    
    val nodeTypeMap = HashMap[String,Int]()
    val edgeTypeMap = HashMap[String,Int]()

    var nNodeTypes = 0
    var nEdgeTypes = 0
    
    var relations = Vector[Relation]()

    var schema = Vector[Tuple3[String,String,String]]()

    def updateSchema(
        tups : Tuple3[String,String,String] *) =
    {
       for ( tup <- tups if (!schema.contains(tup))) schema = schema :+ tup
    }
   
    def updateSchema(
        tups : => Tuple3[ElementType,ElementType,ElementType] *) =
    {
        
        for ((s,p,o) <- tups if (!schema.contains((s.name,p.name,o.name))))
            schema = schema :+ (s.name,p.name,o.name)
    }
    
    def addSchema(_schema : Vector[Tuple3[String,String,String]]) =
    {
        for(tup <- _schema if (!schema.contains(tup))) schema = schema :+ tup
    }
    
    def addSchema(_schema : => Vector[Tuple3[ElementType,ElementType,ElementType]]) =
    {
        for((s,p,o) <- _schema if (!schema.contains((s,p,o))))
            schema = schema :+ (s.name,p.name,o.name)
    }
    def addNodeTypes(elementTypes : ElementType*) =
    {
        elementTypes.foreach( elementType =>
        {
            nodeTypeMap.get( elementType.name ) match
            {
                case None =>
                {
                	nodeTypeMap.update(elementType.name,nNodeTypes)
            		nodeTypes = nodeTypes :+ elementType
            		nNodeTypes = nNodeTypes + 1
                }
                case Some(i) => {}
            }
        })
    }

    def getNodeTypes() : Vector[ElementType] =
    {
        return nodeTypes ++ Vector[ElementType]()
    }

    def containsNodeType(et : ElementType) =
    {
        nodeTypes.map(nt => nt.name).contains(et)
    }

    def addEdgeTypes(elementTypes : ElementType*) =
    {
        elementTypes.foreach( elementType =>
        {
            edgeTypeMap.get( elementType.name ) match
            {
                case None =>
                {
                	edgeTypeMap.update(elementType.name,nEdgeTypes)
            		edgeTypes = edgeTypes :+ elementType
            		nEdgeTypes = nEdgeTypes + 1
                }
                case Some(i) => {}
            }
        })
    }

    def getEdgeTypes() : Vector[ElementType] =
    {
        return edgeTypes ++ Vector[ElementType]()
    }

    def containsEdgeType(et : ElementType) =
    {
        edgeTypes.map(et => et.name).contains(et.name)
    }

    def addRelation(relation : Relation) =
    {
        if (!relations.contains(relation)) relations = relations :+ relation
    }
    
    def addRelation(s : Element, o : Element, p : Element) =
    {
        addRelations(Relation(s,o,p))
    }
    
    def addRelations(_relations : Relation*) =
    {
        for(relation <-_relations if (!relations.contains(relation)))
            relations = relations :+ relation
    }

    def getRelations() : Vector[Relation] = relations ++ Vector[Relation]()

    /*
        Get the elements according to the element types from the map
        Note this does not return any relations
    */
    def apply(_elementTypes : ElementType *) : Graph =
    {
    
        val g = new Graph()
        val elementTypes = {
            if (_elementTypes.length == 0) this.nodeTypes ++ this.edgeTypes
            else _elementTypes
        }
    
        elementTypes.foreach(
            elementType =>
            {
                nodeTypeMap.get(elementType.name) match
                {
                    case Some(i) =>
                    {
                        g.addNodeTypes(this.nodeTypes(i).intersect(elementType))
                    }
                    case None => 
                }
                edgeTypeMap.get(elementType.name) match
                {
                    case Some(i) =>
                    {
                        g.addEdgeTypes(this.edgeTypes(i).intersect(elementType))
                    }
                    case None => //don't do nothing...
                }
            }
        )

        
        val names = elementTypes.map(et=>et.name)
        for ( (s,p,o) <- schema ) {
            if (names.contains(s) && names.contains(p) && names.contains(o))
                 g.updateSchema((s,p,o))
        }
        return g
    }//nodes(nodeTypes:_*)
    
    def nodes(_nodeTypes : ElementType *) : Graph = 
    {
        val g = new Graph()

        val nodeTypes = {
            if (_nodeTypes.length == 0) this.nodeTypes
            else _nodeTypes
        }

        nodeTypes.foreach( 
            nodeType =>
            {
                nodeTypeMap.get(nodeType.name) match
                {
                    case Some(i) =>
                    {
                        g.addNodeTypes(this.nodeTypes(i).intersect(nodeType))
                    }
                    case None => 
                }
            }
        )
        
        return g
    }

    def edges(_edgeTypes : ElementType *) : Graph = 
    {
        val g = new Graph()
        
        val edgeTypes = {
            if (_edgeTypes.length == 0) this.edgeTypes
            else _edgeTypes
        }
        edgeTypes.foreach(
            edgeType =>
            {
                edgeTypeMap.get(edgeType.name) match
                {
                    case Some(i) =>
                    {
                        g.addEdgeTypes(this.edgeTypes(i).intersect(edgeType))
                    }
                    case None => 
                }
            }
        )
        return g
    }

    def paths( schemas : Tuple3[ElementType,ElementType,ElementType]) : Graph =
    {
        return new Graph()
    }

    /*  Expands a graph forward along particular edge types relative to
     *  another graph.
     *  Updates the schema and NodeTypes as necessary:
     *      -   if the argument graph contains schema entry (s,p,o)
     *          where s is a NodeType in the calling object
     *          and p is an edgeType in the _edgeTypes argument,
     *          then o.empty will be added to the calling object as a
     *          NodeType and (s,p,o) will be added to the schema of
     *          the calling object
     */
    def extendForward( g : Graph, _edgeTypes : ElementType *) : Graph = 
    {
        extend(g,FORWARD,_edgeTypes:_*)
    }

    /*  Expands a graph backward along particular edge types relative to
     *  another graph.
     *  Updates the schema and NodeTypes as necessary:
     *      -   if the argument graph contains schema entry (s,p,o)
     *          where s is a NodeType in the calling object
     *          and p is an edgeType in the _edgeTypes argument,
     *          then o.empty will be added to the calling object as a
     *          NodeType and (s,p,o) will be added to the schema of
     *          the calling object
     */
    def extendBackward( g : Graph, _edgeTypes : ElementType *) : Graph = 
    {
        extend(g,BACKWARD,_edgeTypes:_*)
    }

    /*  Expands a graph along particular edge types relative to
     *  another graph.
     *  Updates the schema and NodeTypes as necessary:
     *      -   if the argument graph contains schema entry (s,p,o)
     *          where s is a NodeType in the calling object
     *          and p is an edgeType in the _edgeTypes argument,
     *          then s.empty or o.empty will be added to the calling object as a
     *          NodeType depending on whether dir is FORWARD or BACKWARD
     *          respectively and (s,p,o) will be added to the schema of the
     *          calling object
     */
    private def extend( g : Graph, dir : Int, _edgeTypes : ElementType *) : Graph = 
    {
        val edgeTypes =                           /* Assume that we want to 
                                                   * expand on all  edge types
                                                   * if none are passed
                                                   */
        {
            if (_edgeTypes.length==0) g.edgeTypes
            else g(_edgeTypes:_*).edgeTypes
        } // edgeTypes

        
                                                   /* We'll use the schema of
                                                    * the other graph since
                                                    * that's what we're using
                                                    * as our extension reference
                                                    */                                                   
        g.schema.filter( (subjType,predType,objType) =>
            {
                
                schemaFilter(
                    dir,subjType,objType)  &&     /* We only want subject/object
                                                   * NodeTypes that are part of
                                                   * this graph.
                                                   */
                edgeTypes.map(typ=>typ.name
                    ).contains(predType)          /* We only want schema tuples
                                                   * with EdgeTypes appropriate
                                                   * to the method call
                                                   */
            }
        ).foreach( tup =>
            {    

                /* First, we need a NodeType for this graph to extend to.
                 * Then we need the subject NodeType for the other graph.
                 */
    
                val thisSType = getSOType(tup,g,dir,SUBJ)  
                val otherSType = g.nodeTypes(g.nodeTypeMap(tup._1))
                
                /* Next, we need an EdgeType for this graph to extend with,
                 * It may or may not yet exist for this graph.
                 * Then we need the EdgeType for the other graph.
                 */
            
                val thisPType = getOrMakeElementType(tup,g,EDGE,PRED)
                val otherPType = g.edgeTypes(g.edgeTypeMap(tup._2))

                /* Now, we need the object NodeTypes. 
                 * It may or may not exist in this graph. 
                 */
    
                val thisOType = getSOType(tup,g,dir,OBJ)
                val otherOType = g.nodeTypes(g.nodeTypeMap(tup._3))

                /* Update the schema of this graph to allow any new relation
                 * types we may need
                 */
                updateSchema((thisSType,thisPType,thisOType))

                if (DEBUG) println(s"schema after: \n${schema}")

                /* Finally, we'll filter the relations from the argument
                 * graph by the ElementTypes we found above, and we'll add
                 * the elements and relations that need to be added into the
                 * calling object graph
                 */

                g.relations.filter( rel => 
                            relationFilter(
                                rel,
                                dir,
                                (thisSType,otherSType),
                                (thisPType,otherPType),
                                (thisOType,otherOType))
                ).foreach( rel =>
                    {
                        if (DEBUG) println(s"""rel: \n(${rel.subj},\n${rel.pred},\n${rel.obj})
                                            |relationFilter(...)
                                            |${relationFilter(rel,dir,(thisSType,otherSType),(thisPType,otherPType),(thisOType,otherOType))}""".stripMargin)
                        dir match{
                            case FORWARD  => {thisPType.add(rel.pred);thisOType.add(rel.obj )}
                            case BACKWARD => {thisPType.add(rel.pred);thisSType.add(rel.subj)}
                        } // match
                        addRelation(rel)
                    } // rel =>
                ) // foreach
            } // tup =>
        ) // foreach
        if (DEBUG) print(s"final relations: ${relations}")
    
        return this
    }

    private def relationFilter(
        rel : Relation,
        dir : Int,
        sTypes : (ElementType,ElementType),
        pTypes : (ElementType,ElementType),
        oTypes : (ElementType,ElementType) ) : Boolean =
    {

        val (thisSType,otherSType) = sTypes
        val (thisPType,otherPType) = pTypes
        val (thisOType,otherOType) = oTypes

        if (DEBUG) println(s"""from the relation filter, ${if (dir==FORWARD) "FORWARD" else "BACKWARD"}
                            |rel.subj: \n${rel.subj}
                            |rel.pred: \n${rel.pred}
                            |rel.obj: \n${rel.obj}
            				|otherSType.contains(rel.subj) : ${otherSType.contains(rel.subj)}
            				|otherPType.contains(rel.pred) : ${otherPType.contains(rel.pred)}
                            |thisOType.contains(rel.obj) : ${thisOType.contains(rel.obj)}""".stripMargin)
        dir match
        {
            case FORWARD =>
            {
                if (DEBUG) println("forward in relation filter")
                return (
            	thisSType.contains(rel.subj)  &&    /* We only want to
            	                                     * add paths to this
            	                                     * graph from nodes
            	                                     * which already
            	                                     * exist in it
            	                                     */
            	otherPType.contains(rel.pred) &&
            	                                    /* Filtering for the
            	                                     * ElementType that
            	                                     * we found previous
            	                                     */
            	
            	otherOType.contains(rel.obj) )       /* Again, filtering.
            	                                     */
            } // case FORWARD
            case BACKWARD =>
            {
                if (DEBUG) {
                    println("backward in the relation filter")
                    println(s"otherSType.contains(rel.subj): ${otherSType.contains(rel.subj)}")
                    println(s"otherPType.contains(rel.pred): ${otherPType.contains(rel.pred)}")
                    println(s"thisOType.contains(rel.obj): ${thisOType.contains(rel.obj)}")
                }
                return (
            	otherSType.contains(rel.subj)  &&   /* Filtering for the
            	                                     * ElementType that
            	                                     * we found previous
            	                                     */
                                
            	otherPType.contains(rel.pred) &&
            	                                    /* Again, filtering.
            	                                     */
            	
            	thisOType.contains(rel.obj) )        /* We only want to
            	                                     * add paths to this
            	                                     * graph from nodes
            	                                     * which already
            	                                     * exist in it
            	                                     */
            } // case BACKWARD
        } // match dir    
    }
    /* We want to filter the tuples in the shema for NodeTypes which
     * are relevant to this graph (i.e. - which the graph contains.)
     */
    private def schemaFilter(dir : Int,subjType : String,objType : String) =
    {
        dir match{

            /* The subject NodeType is the relevant NodeType
             * if we want to extend FORWARD.
             */
            case FORWARD => nodeTypeMap.contains(subjType)
                                           
            /* The object NodeType is the relevant NodeType
             * if we want to extend BACKWARD.
             */
            case BACKWARD => nodeTypeMap.contains(objType)        
        }
    }

    /* This method either retrieves or creates a NodeType to extend
     * to based on the direction that we're trying to extend in and
     * which end of the extension we're concerned with.
     */
    private def getSOType(  tup : Tuple3[String,String,String],
                            g   : Graph,
                            dir : Int,
                            typ : Int ) =
    {
        dir match{
            case FORWARD => typ match{
                case SUBJ => nodeTypes(                  
                                nodeTypeMap(tup._1)) /* We know that the subject
                                                      * NodeType exists in this
                                                      * graph because we filtered
                                                      * for it.
                                                      */
                case OBJ => getOrMakeElementType(
                                tup,g,NODE,OBJ)      /* We may have to make a new
                                                      * ElementType if we're
                                                      * expanding forward and we
                                                      * need an object NodeType
                                                      */
            } // case FORWARD
                    
            case BACKWARD => typ match{
                case SUBJ => getOrMakeElementType(  
                                tup,g,NODE,SUBJ)      /* We may have to make a new
                                                      * ElementType if we're
                                                      * expanding forward and we
                                                      * need an object NodeType
                                                      */
                case OBJ => nodeTypes(
                                nodeTypeMap(tup._3)) /* We know that the object
                                                      * NodeType exists in this
                                                      * graph because we filtered
                                                      * for it.
                                                      */
    
            } // case BACKWARD
        }
    }

    /* A method to either get an element type from the appropriate
     * elementTypes vector (i.e. - either nodeTypes or edgeTypes)
     * or make an element type and add it to the elementTypes.
     * tup is the schema tuple we're using to direct our elementType
     * retrieval, g is the graph we're extending with respect to,
     * typ1 tells us if were wanting NodeType or ElementType objects,
     * and typ2 tells us whether we're concerned with the subj, pred,
     * or obj part of the tuple.
     */
    private def getOrMakeElementType(
        tup  : Tuple3[String,String,String],
        g    : Graph,
        typ1 : Int,
        typ2 : Int ) =
    {
        
        val (map,gMap,types,gTypes) = typ1 match
        {
            /*We want nodeTypes and nodeTypeMaps if were dealing with NODE*/
            case NODE => (  nodeTypeMap,
                            g.nodeTypeMap,
                            nodeTypes,
                            g.nodeTypes)

            /*We want edgeTypes and edgeTypeMaps if were dealing with EDGE*/
            case EDGE => (  edgeTypeMap,
                            g.edgeTypeMap,
                            edgeTypes,
                            g.edgeTypes)
        } // match
        

        val tupPart = typ2 match
        {
            case SUBJ => tup._1
            case PRED => tup._2
            case OBJ  => tup._3
        } // match
    
        map.get(tupPart) match
        {
            case Some(i) => types(i)  /* i.e. - the type existed
                                       */
    
            case None =>              /* i.e. - We need to make a new, empty NodeType for 
                                       * nodes to extend to and add it to this graph
                                       */                                    
            {
                val eType = gTypes(gMap(tupPart)).empty()
                typ1 match{
                    case NODE => addNodeTypes(eType)
                    case EDGE => addEdgeTypes(eType)
                } // match
                eType    
            } // None
        } // match
    } // getOrMakeElementType
    
    
    override def toString() : String =
    {
        var str = "\t\tNODES: \n"
        str = str + nodeTypes.foldLeft("")((x,y)=>x+y.toString()+"\n")
        str = str + "\t\tEDGES:\n"
        str = str + edgeTypes.foldLeft("")((x,y)=>x+y.toString()+"\n")
        str = str + "\t\tRELATIONS:\n"
        for ( (_nt1,_et,_nt2) <- schema ){
            val (nt1,et,nt2) =
                (   nodeTypes(nodeTypeMap(_nt1)),
                    edgeTypes(edgeTypeMap(_et )),
                    nodeTypes(nodeTypeMap(_nt2))   )
            str = str + relationTable(nt1,et,nt2) + "\n"
        } 
        return str
    }
    
    private def relationTable(nt1 : ElementType, et : ElementType, nt2 : ElementType) : String =
    {
        val relations = this.relations.filter( relation =>
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

/*
    GraphTesterOne tests the basic functionality of a graph:
        nodes,
*/

object GraphTesterOne extends App{

    class Person(id : Int) extends Node(id){}
    object Person extends NodeType("Person"){}
    
    Person.addProperty(
        ("name",Int)
    )    

    val p1 = Person(("name",1))
    val p2 = Person(("name",2))
    val p3 = Person(("name",3))
    val p4 = Person(("name",4))
    val p5 = Person(("name",5))

    class Movie(id : Int) extends Node(id){}
    object Movie extends NodeType("movie"){}
    
    Movie.addProperty(("title",Int))

    val m1 = Movie(
        ("title",1)
    )
    val m2 = Movie(
        ("title",2)
    )

    class ActedIn(id : Int) extends Edge(id){}
    object ActedIn extends EdgeType("acted_in"){}
    
    ActedIn.addProperty(("role",Int))
    
    val r1  = ActedIn(("role",0))
    val r21 = ActedIn(("role",1))
    val r22 = ActedIn(("role",2))
    val r41 = ActedIn(("role",3))
    val r42 = ActedIn(("role",4))
    
    class Directed(id : Int) extends Edge(id){}
    object Directed extends EdgeType("directed"){}
    
    Directed.addProperty("something",Int)
    val d3 = Directed(("something",3))
    val d5 = Directed(("something",5))
    
    
    val rel1        = Relation(p1,r1,m1)
    val rel2_21_1   = Relation(p2,r21,m1)
    val rel2_22_2   = Relation(p2,r22,m2)
    val rel4_41_1   = Relation(p4,r41,m1)
    val rel4_42_2   = Relation(p4,r42,m2)

    val rel3_3_1    = Relation(p3,d3,m1)
    val rel5_5_2    = Relation(p5,d5,m2)

    println(s"rel1: ${rel1}")
    println(s"rel2_21_1: ${rel2_21_1}")
    println(s"rel2_22_2: ${rel2_22_2}")
    println(s"rel4_41_1: ${rel4_41_1}")
    println(s"rel4_42_2: ${rel4_42_2}")

    val movieDB = new Graph()
    
    movieDB.updateSchema(
        (Person,ActedIn,Movie),
        (Person,Directed,Movie)
    )
    movieDB.addNodeTypes(Person,Movie)
    movieDB.addEdgeTypes(ActedIn,Directed)
    movieDB.addRelations(
                rel1  ,             
    			rel2_21_1,   
    			rel2_22_2,   
    			rel4_41_1,   
    			rel4_42_2,  			
    			rel3_3_1 ,    
    			rel5_5_2)
    
    val g2 = movieDB.nodes()
    val g3 = movieDB(Person)
    val g4 = movieDB(Movie)
    val g5 = movieDB(Person.select("name",_==1))
    val g6 = movieDB(ActedIn)
    val g7 = movieDB(Person.select("name",_==1),Movie.select("title",_==1))

    println(s"movieDB : \n${movieDB}")
    
    println(s"g2 : \n$g2")
    println(s"g3 : \n$g3")
    println(s"g4 : \n$g4")
    println(s"g5 : \n$g5")
    println(s"g6 : \n$g6")
    println(s"g7 : \n$g7")

    /*
    val g8 = movieDB(Person).extendForward(movieDB,ActedIn)
    print(s"g8 : \n${g8}")
    */
    
    val g9 = movieDB(Movie).extendBackward(movieDB,ActedIn)
    print(s"extending the Movie elements backward along ActedIn : \n${g9}")
}

object GraphTesterExtend extends App{

    class Person(id : Int) extends Node(id){}
    object Person extends NodeType("Person"){}
    
    Person.addProperty(
        ("name",Int)
    )    

    val p1 = Person(("name",1))
    val p2 = Person(("name",2))
    val p3 = Person(("name",3))
    val p4 = Person(("name",4))
    val p5 = Person(("name",5))

    class Movie(id : Int) extends Node(id){}
    object Movie extends NodeType("movie"){}
    
    Movie.addProperty(("title",Int))

    val m1 = Movie(
        ("title",1)
    )
    val m2 = Movie(
        ("title",2)
    )

    class ActedIn(id : Int) extends Edge(id){}
    object ActedIn extends EdgeType("acted_in"){}
    
    ActedIn.addProperty(("role",Int))
    
    val r1  = ActedIn(("role",0))
    val r21 = ActedIn(("role",1))
    val r22 = ActedIn(("role",2))
    val r41 = ActedIn(("role",3))
    val r42 = ActedIn(("role",4))
    
    class Directed(id : Int) extends Edge(id){}
    object Directed extends EdgeType("directed"){}
    
    Directed.addProperty("something",Int)
    val d3 = Directed(("something",3))
    val d5 = Directed(("something",5))
    
    
    val rel1        = Relation(p1,r1,m1)
    val rel2_21_1   = Relation(p2,r21,m1)
    val rel2_22_2   = Relation(p2,r22,m2)
    val rel4_41_1   = Relation(p4,r41,m1)
    val rel4_42_2   = Relation(p4,r42,m2)

    val rel3_3_1    = Relation(p3,d3,m1)
    val rel5_5_2    = Relation(p5,d5,m2)
    
    val movieDB = new Graph()
    
    movieDB.updateSchema(
        (Person,ActedIn,Movie),
        (Person,Directed,Movie)
    )
    movieDB.addNodeTypes(Person,Movie)
    movieDB.addEdgeTypes(ActedIn,Directed)
    movieDB.addRelations(
                rel1  ,             
    			rel2_21_1,   
    			rel2_22_2,   
    			rel4_41_1,   
    			rel4_42_2,  			
    			rel3_3_1 ,    
    			rel5_5_2)

    
    val g8 = movieDB(Person).extendForward(movieDB,ActedIn)
    print(s"g8 : \n${g8}")
    

    println(s"movieDB: \n${movieDB}")
    val g9 = movieDB(Movie).extendBackward(movieDB,ActedIn)
    print(s"extending the Movie elements backward along ActedIn : \n${g9}")
}