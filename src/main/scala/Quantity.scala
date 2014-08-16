package scunits.quantity

import scunits._
import scunits.integer._
import scunits.tlist._

trait Quantities extends TList[BaseQuantity] {
  type Quantities <: TEl[BaseQuantity]

  trait Zeroer extends TMap[BaseQuantity] {
    type Out = Integer
    type Apply[Q <: BaseQuantity] = _0
  }

  type Base = Quantity[Quantities, Quantities#Map[Zeroer]]
}

object Physical extends Quantities {

  object Length extends BaseQuantity("length", "L")
  object Time extends BaseQuantity("time", "T")
  object Mass extends BaseQuantity("mass", "M")
  object Temperature extends BaseQuantity("temperature", "Θ")
  object AmountOfSubstance extends BaseQuantity("mole", "N")
  object ElectricCurrent extends BaseQuantity("electric current", "I")
  object LuminousIntensity extends BaseQuantity("luminous intensity", "J")
  object Angle extends BaseQuantity("angle", "")
  object SolidAngle extends BaseQuantity("solid angle", "")

  type Quantities =
    Length.type :: Time.type :: Mass.type :: Temperature.type :: AmountOfSubstance.type ::
    ElectricCurrent.type :: LuminousIntensity.type :: Angle.type :: SolidAngle.type :: TNil[BaseQuantity]

  // type *[L <: Quantity[Quantities,_ <: TEl[Integer]], R <: Quantity[Quantities,_ <: TEl[Integer]]] = L#Mult[R]
}