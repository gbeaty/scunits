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
  type Quants <: QList

  type Dims                   = types.DimsOf[Quants]
  type DimsOf[D <: EList]     = Quants ^ D
  type DimOf[I <: NonNegInt]  = DimsOf[ENil#Set[I,i1]]
  type MeasOf[I <: NonNegInt] = Measure[DimsOf[ENil#Set[I,i1]]]
  type Dimless                = DimsOf[ENil]

  val coef = UnitM[Dimless](mult = 1.0)
  implicit def toCoef(v: Double) = Measure[Dimless](v)
}