package scunits

import scunits.tlist._
import scunits.integer._

trait Quantity
class BaseQuantity(val name: String, val symbol: String) extends Quantity

object Length extends BaseQuantity("length", "L")
object Time extends BaseQuantity("time", "T")
object Mass extends BaseQuantity("mass", "M")
object Temperature extends BaseQuantity("temperature", "Θ")
object AmountOfSubstance extends BaseQuantity("mole", "N")
object ElectricCurrent extends BaseQuantity("electric current", "I")
object LuminousIntensity extends BaseQuantity("luminous intensity", "J")
object Angle extends BaseQuantity("angle", "")
object SolidAngle extends BaseQuantity("solid angle", "")

object QList extends TList[BaseQuantity]
import QList._

trait BaseQuantities {
  type Quantities <: TEl[BaseQuantity]

  trait Zeroer extends TMap[BaseQuantity] {
    type Out = Integer
    type Apply[Q <: BaseQuantity] = _0
  }

  type Base = Dimension[Quantities, Quantities#Map[Zeroer]]
}

object PhysicalQuantities extends BaseQuantities {
  type Quantities =
    Length.type :: Time.type :: Mass.type :: Temperature.type :: AmountOfSubstance.type ::
    ElectricCurrent.type :: LuminousIntensity.type :: Angle.type :: SolidAngle.type :: TNil[BaseQuantity]
}

package object quantity {
  type Exponents = TEl[Integer]
  type Quantities = TEl[BaseQuantity]
}

trait Dimension[Q <: TEl[BaseQuantity], E <: TEl[Integer]] {
  type Quantities = Q
  type Exponents = E
  type Mult[R <: Dimension[Q,_ <: TEl[Integer]]] = Dimension[Q,E#Zip[R#Exponents,Integer]#Map[Op[+]]]
  type Div[R <: Dimension[Q,_ <: TEl[Integer]]] = Dimension[Q,E#Zip[R#Exponents,Integer]#Map[Op[-]]]
}