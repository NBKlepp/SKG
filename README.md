# SKG

SKG is a Knowledeg Graph software package written for the Scalation software
package.

The data model is a directed property label graph, where nodes are related to
one another via directed edges. Both nodes and edges are labeled via NodeType and
EdgeType objects, and both Nodes and Edges can be annotated with properties.
Properties are [K,V] pairs where the K is a string and V is a Primitive Value
defined in the Scalation software package (integers, floating point values,
complex values, strings).

See the GraphTesterPaths object for an implementation of the following
examples...

Consider the task of creating a knowledge graph with the IMDB (Internet Movie
Data Base) data. The nodes in our graph would consist of Person and Movie nodes
(people and movies respectively). People should have names (a name property), and
movies should have titles (a title propety). People are related to the movies
that they have acted in via the ActedIn edges which would store the information
about the role the actor had in the movie - and people are related to the movies
they direct via the Directed edges which have no properties.

Here is an example of creating a Person node type:

```
    class Person(id : Int) extends Node(id){}
    object Person extends NodeType("Person"){}
```

The `Person` class extends the `Node` class, which means it has a node id and it
may be annotated with [K,V] property pairs. The 'Person' companion object extends
the `NodeType` trait with the label `Person`. We can keep track of the Person
nodes via the Person companion object. We can also add a property to the Person
class via the Person companion object:

```
Person.addProperty(
    ("name",String)
) 
```

This adds the name property to the `Person` Node type and specifies it's domain as
String. Now we can create `Person` nodes with names:

```
val CharlieSheen   = Person(("name","Charlie Sheen"))
val MartinSheen    = Person(("name","Martin Sheen"))
val OliverStone    = Person(("name","Oliver Stone"))
val MichaelDouglas = Person(("name","Michael Douglas"))
val RobReiner      = Person(("name","Rob Reiner"))
```

We can do something similar to create movie nodes with properties, property
domains, property values, etc.:

```
class Movie(id : Int) extends Node(id){}
object Movie extends NodeType("movie"){}

Movie.addProperty(("title",String))

val WallStreet = Movie(
    ("title","Wall Street")
)
val AmericanPres = Movie(
    ("title","The American President")
)
```

Note that creating the nodes through the factory methods of the NodeType objects
we are automatically indexing the nodes via the label of the NodeType.

Now we'll set up the EdgeTypes, but we'll wait to instantiate the edge for a
moment:

```
class ActedIn(id : Int) extends Edge(id){}
object ActedIn extends EdgeType("acted_in"){}

ActedIn.addProperty(("role",String))

class Directed(id : Int) extends Edge(id){}
object Directed extends EdgeType("directed"){}
```

Next we will create a graph object, update the schema of the graph to reflect
the relational schema of people to movies via the Directed and ActedIn
edges:

'''
val movieDB = new Graph()

movieDB.updateSchema(
    (Person,ActedIn,Movie),
    (Person,Directed,Movie)
)
movieDB.addNodeTypes(Person,Movie)
movieDB.addEdgeTypes(ActedIn,Directed)
```

From here, we will relate the nodes to one another according to the appropriate
edge types. The spacing below is merely for legibility:

```
movieDB.relate(
    (
        OliverStone,
        Directed(),
        WallStreet
    ),                            	    
	(
        RobReiner,
        Directed(),
        AmericanPres
    ),                          	
	(
        CharlieSheen, 
        ActedIn(("role","Bud Fox")),
        WallStreet
    ),
	(
        MartinSheen,
        ActedIn(("role","Carl Fox")),
        WallStreet
    ),
	(
        MartinSheen,
        ActedIn(("role","A.J. MacInerney")),
        AmericanPres
    ),
	(
        MichaelDouglas,
        ActedIn(("role","President Andrew Shepherd")),
        AmericanPres
    ),
	(
        MichaelDouglas,
        ActedIn(("role","Gordon Gekko")),
        WallStreet
    )
)
```

Now our knowledge graph is complete and we can query on it. Firstly, we can
select the nodes from the `Person` node type where the node's `name` property
have "Michael Douglas" for their value, i.e. - the Michael Douglas nodes:

```
    Person.select("name",_=="Michael Douglas")
```
Next, we can query for the subgraph of the movie data base with paths relating
the Michael Douglas node to movie nodes that he acted in:

```
movieDB.paths( Person.select("name",_=="Michael Douglas"), ActedIn, Movie)
```

If we want just the *movies* that Michael Douglas acted in, we "project" the above
graph onto just the movie nodes:

```
movieDB.paths( Person.select("name",_=="Michael Douglas"), ActedIn, Movie)(Movie)
```

The `selectOr` method of the `NodeType` trait allows us to select for a
disjunction of predicates instead of the single predicate we used in the `select`
method above. For instance,  we can query for all of the movies that either
Michael Douglas or Martin Sheen acted in:

```
movieDB.paths(
    Person.selectOR(
        ("name",_=="Michael Douglas"),
        ("name",_=="Martin Sheen")),
    ActedIn,
    Movie)(Movie)
```

Finally, to find the movies that either Michael Douglas acted in or Oliver Stone
directed, we would use the following:

```
movieDB.paths(
    (
        Person.select("name",_=="Michael Douglas"),
        ActedIn,
        Movie
    ),
    (
        Person.select("name",_=="Oliver Stone"),
        Directed,
        Movie
    )(Movie)
)

The results of our queries are below:

***************the movieDB graph database, as a reference*********************
    
	movieDB: 
			NODES: 
	+------------------------+
	+         PERSON         +
	+-----------------+------+
	+      NAME       +  ID  +
	+-----------------+------+
	+  Charlie Sheen  +  0   +
	+-----------------+------+
	+  Martin Sheen   +  1   +
	+-----------------+------+
	+  Oliver Stone   +  2   +
	+-----------------+------+
	+ Michael Douglas +  3   +
	+-----------------+------+
	+   Rob Reiner    +  4   +
	+-----------------+------+
	
	+------------------------------+
	+            MOVIE             +
	+-----+------------------------+
	+ ID  +         TITLE          +
	+-----+------------------------+
	+  0  +      Wall Street       +
	+-----+------------------------+
	+  1  + The American President +
	+-----+------------------------+
	
			EDGES:
	+------------------------------------+
	+              ACTED_IN              +
	+--------+---------------------------+
	+   ID   +           ROLE            +
	+--------+---------------------------+
	+   0    +          Bud Fox          +
	+--------+---------------------------+
    +   1    +          Carl Fox         +
    +--------+---------------------------+
    +   2    +      A.J. MacInerney      +
    +--------+---------------------------+
    +   3    + President Andrew Shepherd +
    +--------+---------------------------+
    +   4    +       Gordon Gekko        +
    +--------+---------------------------+

    +--------+
    +DIRECTED+
    +--------+
    +   ID   +
    +--------+
    +   0    +
    +--------+
    +   1    +
    +--------+

            RELATIONS:

    (Person)--[acted_in]-->(movie)
    (0)--[0]-->(0)
    (1)--[1]-->(0)
    (1)--[2]-->(1)
    (3)--[3]-->(1)
    (3)--[4]-->(0)


    (Person)--[directed]-->(movie)
    (2)--[0]-->(0)
    (4)--[1]-->(1)

************getting the paths about Michael Douglas's movies******************

            NODES:
    +------------------------+
    +         PERSON         +
    +-----------------+------+
    +      NAME       +  ID  +
    +-----------------+------+
    + Michael Douglas +  3   +
    +-----------------+------+

    +------------------------------+
    +            MOVIE             +
    +-----+------------------------+
    + ID  +         TITLE          +
    +-----+------------------------+
    +  1  + The American President +
    +-----+------------------------+
    +  0  +      Wall Street       +
    +-----+------------------------+

            EDGES:
    +------------------------------------+
    +              ACTED_IN              +
    +--------+---------------------------+
    +   ID   +           ROLE            +
    +--------+---------------------------+
    +   3    + President Andrew Shepherd +
    +--------+---------------------------+
    +   4    +       Gordon Gekko        +
    +--------+---------------------------+

            RELATIONS:

    (Person)--[acted_in]-->(movie)
    (3)--[3]-->(1)
    (3)--[4]-->(0)

*****the movies either Michael Douglas or Martin Sheen acted in **************

            NODES:
    +------------------------+
    +         PERSON         +
    +-----------------+------+
    +      NAME       +  ID  +
    +-----------------+------+
    +  Martin Sheen   +  1   +
    +-----------------+------+
    + Michael Douglas +  3   +
    +-----------------+------+

    +------------------------------+
    +            MOVIE             +
    +-----+------------------------+
    + ID  +         TITLE          +
    +-----+------------------------+
    +  0  +      Wall Street       +
    +-----+------------------------+
    +  1  + The American President +
    +-----+------------------------+

            EDGES:
    +------------------------------------+
    +              ACTED_IN              +
    +--------+---------------------------+
    +   ID   +           ROLE            +
    +--------+---------------------------+
    +   1    +         Carl Fox          +
    +--------+---------------------------+
    +   2    +      A.J. MacInerney      +
    +--------+---------------------------+
    +   3    + President Andrew Shepherd +
    +--------+---------------------------+
    +   4    +       Gordon Gekko        +
    +--------+---------------------------+

            RELATIONS:

    (Person)--[acted_in]-->(movie)
    (1)--[1]-->(0)
    (1)--[2]-->(1)
    (3)--[3]-->(1)
    (3)--[4]-->(0)

*****the movies either Michael Douglas acted in or Oliver Stone Directed******

            NODES:
    +------------------------+
    +         PERSON         +
    +-----------------+------+
    +      NAME       +  ID  +
    +-----------------+------+
    + Michael Douglas +  3   +
    +-----------------+------+
    +  Oliver Stone   +  2   +
    +-----------------+------+

    +------------------------------+
    +            MOVIE             +
    +-----+------------------------+
    + ID  +         TITLE          +
    +-----+------------------------+
    +  1  + The American President +
    +-----+------------------------+
    +  0  +      Wall Street       +
    +-----+------------------------+

            EDGES:
    +------------------------------------+
    +              ACTED_IN              +
    +--------+---------------------------+
    +   ID   +           ROLE            +
    +--------+---------------------------+
    +   3    + President Andrew Shepherd +
    +--------+---------------------------+
    +   4    +       Gordon Gekko        +
    +--------+---------------------------+

    +--------+
    +DIRECTED+
    +--------+
    +   ID   +
    +--------+
    +   0    +
    +--------+

            RELATIONS:

    (Person)--[acted_in]-->(movie)
    (3)--[3]-->(1)
    (3)--[4]-->(0)


    (Person)--[directed]-->(movie)
    (2)--[0]-->(0)
    