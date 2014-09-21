package scunits

import scunits.types._

sealed trait Quantity
trait BaseQuantity extends Quantity
sealed trait AbstractQuantity extends Quantity

object Time extends BaseQuantity
object Info extends BaseQuantity
object Length extends BaseQuantity
object Mass extends BaseQuantity
object Temperature extends BaseQuantity
object AmountOfSubstance extends BaseQuantity  
object Current extends BaseQuantity
object Intensity extends BaseQuantity

class QuantConverter[From <: QList, Ids <: DList]

trait Quantities {
  type Quants <: QList

  type Dims                  = types.DimsOf[Quants]
  type DimsOf[D <: DList]    = Quants ^ D
  type DimOf[I <: NonNegInt] = DimsOf[DNil#Set[I,i1]]
  type Dimless               = DimsOf[DNil]

  val coef = UnitM[Dimless](mult = 1.0)
  implicit def toCoef(v: Double) = Measure[Dimless](v)
}