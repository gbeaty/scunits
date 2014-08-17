package scunits2

import scunits.tlist2._
import scunits.integer._
import scunits.bool._
import scunits.integer.Ops._

trait BaseDim[I <: NonNegInt, M <: Integer] {
  type Index = I
  type Mag = M
  type Mult[R <: BaseDim[I,_ <: Integer]] = BaseDim[I,M + R#Mag]
  type Div[R <: BaseDim[I,_ <: Integer]] = BaseDim[I,M - R#Mag]
}

object Dims {
  type SomeDim = BaseDim[_ <: NonNegInt, _ <: NonZeroInt]
  type SomeDims = Dim[_ <: SomeDim, _ <: Dims]
}
import Dims._
trait Dims {
  type Empty <: Bool
  type Mult[R <: SomeDim]
}
trait Dim[H <: SomeDim, T <: Dims] extends Dims {
  type Empty = False
  // type Test = (H#Index > H#Mag)#Ifs[Int,Int,Int]
  // val t: Test = 2
  private type HmR[R <: SomeDim, RM <: Integer] = RM#IsPos#If[Dim[R,this.type],this.type]
  type Mult[R <: SomeDim] = HmR[R, H#Index - R#Index]
}
trait DimNil extends Dims {
  type Empty = True
  type Mult[R <: SomeDim] = Dim[R,DimNil]
}

class BaseQuantity[I <: NonNegInt](val name: String, val symbol: String) extends BaseDim[I,_1]

object Quantities {
  object Length extends BaseQuantity[_0]("length", "L")
  object Time extends BaseQuantity[_1]("time", "T")
  object Mass extends BaseQuantity[_2]("mass", "M")
  object Temperature extends BaseQuantity[_3]("temperature", "Θ")
  object AmountOfSubstance extends BaseQuantity[_4]("mole", "N")
  object ElectricCurrent extends BaseQuantity[_5]("electric current", "I")
  object LuminousIntensity extends BaseQuantity[_6]("luminous intensity", "J")
  object Angle extends BaseQuantity[_7]("angle", "")
  object SolidAngle extends BaseQuantity[_8]("solid angle", "")
  object Bit extends BaseQuantity[_9]("bit", "b")
}