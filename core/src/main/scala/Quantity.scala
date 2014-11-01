package scunits

import scunits.types._

trait Quantity
trait BaseQuantity extends Quantity
trait AbstractQuantity extends Quantity

class Quantities {
  type quants <: QList

  type Dims                   = types.DimsOf[quants]
  type dimsOf[D <: EList]     = quants ^ D
  type dimOf[I <: NonNegInt]  = dimsOf[ENil#set[I,p1]]
  type Dimless                = dimsOf[ENil]

  val coef = UnitM[Dimless](mult = 1.0)
  implicit def toCoef(v: Double) = Scalar[Dimless](v)
}