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

  type dims                   = types.DimsOf[quants]
  type dimsOf[D <: EList]     = quants ^ D
  type dimOf[I <: NonNegInt]  = dimsOf[ENil#Set[I,i1]]
  type Dimless                = dimsOf[ENil]

  val coef = UnitM[Dimless](mult = 1.0)
  implicit def toCoef(v: Double) = Measure[Dimless](v)
}