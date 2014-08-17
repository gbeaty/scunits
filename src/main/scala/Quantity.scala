package scunits.quantity

import scunits._
import scunits.integer._
import scunits.tlist._

trait Quantities[Qs <: BaseQuantities] {

  trait Zeroer extends TMap[BaseQuantity] {
    type Out = Integer
    type Apply[Q <: BaseQuantity] = _0
  }

  type Base = Dimensions[Qs, Qs#Map[Zeroer]]

  // Referencing the * and / type operators in Dimensions won't compile for some reason.
  type *[L <: DimensionsOf[Qs], R <: DimensionsOf[Qs]] = Dimensions[Qs,L#Mags#Zip[R#Mags,Integer]#Map[Op[+]]]
  type /[L <: DimensionsOf[Qs], R <: DimensionsOf[Qs]] = Dimensions[Qs,L#Mags#Zip[R#Mags,Integer]#Map[Op[-]]]
}

object Physical {
  class Length extends BaseQuantity("length", "L")
  class Time extends BaseQuantity("time", "T")
  class Mass extends BaseQuantity("mass", "M")
  class Temperature extends BaseQuantity("temperature", "Θ")
  class AmountOfSubstance extends BaseQuantity("mole", "N")
  class ElectricCurrent extends BaseQuantity("electric current", "I")
  class LuminousIntensity extends BaseQuantity("luminous intensity", "J")
  class Angle extends BaseQuantity("angle", "")
  class SolidAngle extends BaseQuantity("solid angle", "")
}
object Information {
  class Bit extends BaseQuantity("bit","b")
}