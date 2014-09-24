import scunits._
import scunits.types._

package object scunits {
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv  

  import Converter._
  @annotation.implicitNotFound(msg = "Cannot generate a Converter of type ${From} -> ${To}. Consider creating one manually.")
  def converter[From <: Quantities, To <: Quantities](implicit cb: Converter[From#Quants, To#Quants]) = cb

  implicit def identityConverter[A <: QList] = new Converter[A,A]
}