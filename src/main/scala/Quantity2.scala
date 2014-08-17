package scunits2

import scunits.tlist2._
import scunits.integer._
import scunits.integer.Ops._

trait DimensionLike {
  type Id <: NonNegInt
  type Mag <: Integer

  type Mult[R <: DimensionLike] = Dimension[Id, Mag + R#Mag]
  type Div[R <: DimensionLike] = Dimension[Id, Mag - R#Mag]
}
trait Dimension[I <: NonNegInt, M <: Integer] extends DimensionLike {
  type Mag = M 
  type Id = I
}

trait DList {
  type Add[R <: DimensionLike] <: DNelLike
  type Mult[R <: DList] <: DList
  protected type MultL[L <: DList] <: DList
}
trait DNelLike extends DList {
  type Head <: DimensionLike
  type Tail <: DList

  type Add[R <: DimensionLike] = (R#Id - Head#Id)#BranchNegZeroPos[
    DNelLike,
    DNel[R,DNel[Head,Tail]],
    DNel[Dimension[Head#Id,Head#Mag + R#Mag],Tail],
    DNel[Head,Tail#Add[R]]
  ]

  type Mult[R <: DList] = R#MultL[DNel[Head,Tail]]
  type MultL[L <: DList] = L#Add[Head]#Mult[Tail]
  // type MultL[L <: DNelLike] = L#Add[Quantities.Length.]
  // DNel[Dimension[_0,_2],DNil]
}
// Length#Mult[Length] = Length#MultL[Length] = Length#Add[Length.Dim]#Mult[DNil]
trait DNel[H <: DimensionLike, T <: DList] extends DNelLike {
  type Head = H
  type Tail = T
}

trait DNil extends DList {
  type Add[R <: DimensionLike] = DNel[R,DNil]
  type Mult[R <: DList] = R
  type MultL[L <: DList] = L
}

class BaseQuantity[I <: NonNegInt](val name: String, val symbol: String) {
  type Id = I
  type Base = DNel[Dim, DNil]
  type Dim = Dimension[Id, _1]
}

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

  type *[L <: DList, R <: DList] = L#Mult[R]

  type Area = Length.Base * Length.Base
  type Volume = Area * Length.Base

  object Electrical {
    type Capacity = ElectricCurrent.Base * Time.Base
  }
  // type Volume = Area * Length.type
}

object Tests {
  import Quantities._

  implicitly[Length.Dim =:= Dimension[_0,_1]]
  implicitly[Length.Dim#Mult[Length.Dim] =:= Dimension[_0,_2]]
  implicitly[Length.Dim#Div[Length.Dim] =:= Dimension[_0,_0]]
  implicitly[Length.Dim#Mult[Length.Dim]#Div[Length.Dim] =:= Dimension[_0,_1]]

  type LengthAndTime = DNel[Length.Dim, DNel[Time.Dim,DNil]]
  implicitly[Length.Base#Add[Time.Dim] =:= LengthAndTime]
  implicitly[Time.Base#Add[Length.Dim] =:= LengthAndTime]

  implicitly[DNil * DNil =:= DNil]
  implicitly[DNil * Length.Base =:= Length.Base]
  implicitly[Length.Base * DNil =:= Length.Base]
  implicitly[Area =:= DNel[Dimension[_0,_2],DNil]]
  implicitly[Volume =:= DNel[Dimension[_0,_3],DNil]]
  implicitly[Electrical.Capacity =:= DNel[Dimension[_1,_1],DNel[Dimension[_5,_1],DNil]]]
  implicitly[Time.Base * ElectricCurrent.Base =:= Electrical.Capacity]

  // DNel[Dimension[_0,_2],DNil]

  // DNel[Length.Dim,DNel[Time.Dim,DNil]]

  // implicitly[Length.Base#Add[Time.Dim] =:= DNel[Dimension[_1,_1], DNil]]

  // Time.Dim#Id#Sub[_0]#BranchNegZeroPos[DNel[Dimension[_0,Time.Dim#Mag#Succ],DNil],DNel[Time.Dim,DNil]]

  // _1#Succ#DoNonZero[[NM <: Integer]Dimension[_0,NM],One]

  // val t: Area = 1

  // implicitly[Length.Base =:= DNil#Insert[Length.Base]]
  // implicitly[Length.Base =:= DNel[_0,_1,DNil]]
  // implicitly[Area =:= DNel[_0,_2,DNil]]
  // implicitly[DNil#Combine[Length.type] =:= DNel[_0,_1,DNil]]
  // implicitly[DNil * Length.type =:= Length.type]
  // implicitly[Length.type * DNil =:= Length.type]
  // implicitly[Area =:= DNel[BaseDim[_0,_2],DNil]]
}