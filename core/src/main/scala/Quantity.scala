package scunits

import scunits.types._

trait BaseQuantity

object Time extends BaseQuantity
object Info extends BaseQuantity
object Length extends BaseQuantity
object Mass extends BaseQuantity
object Temperature extends BaseQuantity
object AmountOfSubstance extends BaseQuantity  
object Current extends BaseQuantity
object Intensity extends BaseQuantity

trait Quantities {
  type quants <: QList

  type Dims                   = types.DimsOf[quants]
  type dimsOf[D <: EList]     = quants ^ D
  type dimOf[I <: NonNegInt]  = dimsOf[ENil#Set[I,i1]]
  type Dimless                = dimsOf[ENil]

  def dimOf[B <: BaseQuantity,E <: EList](b: B)(implicit d: DimensionOf[quants,B,E]) = d

  val coef = UnitM[Dimless](mult = 1.0)
  implicit def toCoef(v: Double) = Measure[Dimless](v)
}

trait Append[Qs <: QNil, To <: Quantities] extends Quantities {
  type quants = To#quants#append[Qs]
}