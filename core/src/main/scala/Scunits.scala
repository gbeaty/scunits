import scunits._
import scunits.types._

package object scunits {
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv

  implicit def convert[F <: QList, T <: QList, D <: DimsOf[F]](m: Measure[D])(implicit c: Converter[F,T]) =
    Measure[c.Apply[D]](m.v)

  import Converter._  
  @annotation.implicitNotFound(msg = "Cannot generate a Converter of type ${F} -> ${T}. Consider creating one manually.")
  def converter[F <: Quantities, T <: Quantities, C <: Converter[F#Quants,T#Quants]](f: F, t: T)(implicit c: C) = c
}