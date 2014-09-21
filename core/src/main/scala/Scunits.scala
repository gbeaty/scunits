import scunits._
import scunits.types._

package object scunits {  
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv  
}