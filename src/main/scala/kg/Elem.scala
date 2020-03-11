package knowledgeGraph
import scala.collection.immutable.Vector
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
//import scalation.math.StrO.StrNum
import scalation.math.TimeO

type PrimitiveType = Real.type | Rational.type | StrO.type   | TimeO.type    | Complex.type | Int.type | Double.type | Long.type
type Primitive     = Real      | Rational      | StrO.StrNum | TimeO.TimeNum | Complex      | Int      | Double      | Long
    
abstract class Elem(id : String){
    val _id = id
    //val properties : Vector[Complex | Rational | Real | StrO | StrNum | TimeO]
    //var properties = Vector[Option[Complex | Rational | Real | StrNum]]()
    var properties = Vector[Primitive]()
}