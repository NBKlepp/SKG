package kg

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
    
type PrimitiveType = Real.type | Rational.type | StrO.type   | TimeO.type    | Complex.type | Int.type | Double.type | Long.type
type Primitive     = Real      | Rational      | StrO.StrNum | TimeO.TimeNum | Complex      | Int      | Double      | Long

class Schema extends HashMap[String,PrimitiveType]