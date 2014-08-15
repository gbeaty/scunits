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

  trait Mapper extends TMap[BaseQuantity] {
    type Out = Integer
    type Apply[Q <: BaseQuantity] = _0
  }

  // type Dimensions = Magnitudes[Quantities, Quantities#Map[Mapper]]
}

object PhysicalQuantities extends BaseQuantities {
  type Quantities =
    Length.type :: Time.type :: Mass.type :: Temperature.type :: AmountOfSubstance.type ::
    ElectricCurrent.type :: LuminousIntensity.type :: Angle.type :: SolidAngle.type :: TNil[BaseQuantity]
}

package object quantity {
  type Exponents = TEl[Integer]
  type Quantities = TEl[BaseQuantity]
  // type *[L <: Exponents, R <: Exponents] = L#Zip[R,Integer]#Map[Op[+]]
  //type Div[R <: Exponents] = Zip[R,Integer]#Map[Op[-]]
}

trait Dimension[Q <: TEl[BaseQuantity], E <: TEl[Integer]] {
  type Quantities = Q
  type Exponents = E
  type Mult[R <: Dimension[Q,_ <: TEl[Integer]]] = Dimension[Q,E#Zip[R#Exponents,Integer]#Map[Op[+]]]
  type Div[R <: Dimension[Q,_ <: TEl[Integer]]] = Dimension[Q,E#Zip[R#Exponents,Integer]#Map[Op[-]]]
  // type Mult[RE <: TEl[Integer]] = Dimension[Q,E#Zip[RE,Integer]#Map[Op[+]]]
}

/*trait Magnitudes[Q <: Quantities, E <: Exponents] {
  type Exponents = E
  type Quantities = Q
}*/

/*trait DerivedQuantity[BQ <: TList[BaseQuantity]] extends TList[Magnitude[_ <: BaseQuantity,_ <: Integer]] {
  
  trait QMap extends TMap[BaseQuantity] {
    type Out[Q <: BaseQuantity] = Magnitude[Q,_0]
  }
}*/

/*object Units {
  import scunits.nums._

  trait Units[U <: Units[U]] {
    type Mult[R <: U] <: Units[U]
    type Div[R <: U] <: Units[U]
  }
  trait UNil extends Units[UNil] {
    type Mult[R <: Units[UNil]] = UNil
    type Div[R <: Units[UNil]] = UNil
  }
  trait UEl[Q <: BaseQuantityEl, M <: Integer, T <: SuccQuantity[_ <: BaseQuantity]] extends Units[UEl[Q,_<:Integer,T]] {
    type Mag = M
    type Mult[R <: UEl[Q,_<:Integer,T]] = UEl[Q, M + R#Mag, T]
    type Div[R <: UEl[Q,_<:Integer,T]] = UEl[Q, M - R#Mag, T]
  }
}*/

/*sealed trait Units[M <: Integer, Q <: BaseQuantityEl] {
  type Magnitude = M
  type Quantity = Q
  // type Result = Units[_ <: Integer, Q]

  type Mult[R <: Units[_ <: Integer,Q]]// <: Units[M + R#Magnitude, Q]
}
sealed trait LastUnit[M <: Integer, Q <: FirstQuantity] extends Units[M,Q] {
  type Mult[R <: LastUnit[_ <: Integer,Q]] = LastUnit[M + R#Magnitude, Q]
}*/
/*trait SuccUnit[M <: Integer, Q <: SuccQuantity[_ <: BaseQuantityEl], T <: Units[_ <: Integer, Q#Prev]] extends Units[M,Q] {
  type Tail = T

  // type Mult[R <: SuccUnit[_ <: Integer, Q, _ <: Units[_ <: Integer, Q]]]
}*/

/*trait Units[M <: Integer, Q <: BaseQuantityEl, T <: Units[_ <: Integer, _, _]] {
  type Magnitude = M
  type Quantity = Q
  type Tail = T

  // type Multiply[R <: Units[_ <: Integer, Q]] = Units[Magnitude#Add[R#Magnitude], Q]
}*/

// abstract class DerivedQuantity[B <: HList](implicit ) extends Quantity

/*abstract class DerivedQuantity[L <: Quantity, R <: Quantity](val left: L, val right: R, op: String)
  extends Quantity("(" + left.name + op + right.name + ")", "(" + left.symbol + op + right.symbol + ")")

class Mul[L <: Quantity, R <: Quantity](l: L, r: R)
  extends DerivedQuantity(l,r,"*")
class Div[L <: Quantity, R <: Quantity](l: L, r: R)
  extends DerivedQuantity(l,r,"/")

object Speed extends Div(Length,Time)
object Acceleration extends Div(Speed,Time)*/
/*trait Multiplier[L <: Quantity, R <: Quantity, E <: Quantity] {
  def apply(l: Measurement[L], r: Measurement[R]) = Measurement[E](l.v * r.v)
}*/