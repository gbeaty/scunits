package scunits

import scunits.tlist2._
import scunits.integer._
import scunits.integer.Ops._

trait DimensionLike {
  type Id <: NonNegInt
  type Mag <: Integer
  type Neg = Dimension[Id,Mag#Neg]
}
trait Dimension[I <: NonNegInt, M <: Integer] extends DimensionLike {
  type Mag = M 
  type Id = I
}

trait DList {
  type Add[R <: DimensionLike] <: DNelLike

  type Mult[R <: DList] = Combine[R]
  type Div[R <: DList] = Combine[R#Neg]

  type Combine[R <: DList] <: DList
  protected type CombineL[L <: DList] <: DList

  type Neg <: DList
}
trait DNelLike extends DList {
  type Head <: DimensionLike
  type Tail <: DList

  type Add[R <: DimensionLike] = (R#Id - Head#Id)#BranchNegZeroPos[
    DNelLike,
    DNel[Dimension[R#Id,R#Mag],DNel[Head,Tail]],
    DNel[Dimension[Head#Id,Head#Mag + R#Mag],Tail],
    DNel[Head,Tail#Add[R]]
  ]

  type Combine[R <: DList] = R#CombineL[DNel[Head,Tail]]
  protected type CombineL[L <: DList] = L#Add[Head]#Combine[Tail]
  type Neg = DNel[Dimension[Head#Id,Head#Mag#Neg],Tail#Neg]
}

trait DNel[H <: DimensionLike, T <: DList] extends DNelLike {
  type Head = H
  type Tail = T
}

trait DNil extends DList {
  type Add[R <: DimensionLike] = DNel[Dimension[R#Id,R#Mag],DNil] 
  type Combine[R <: DList] = R
  protected type CombineL[L <: DList] = L
  type Neg = DNil
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
  object LuminousIntensity extends BaseQuantity[_6]("luminous intensity", "J")
  object Angle extends BaseQuantity[_7]("angle", "")
  object SolidAngle extends BaseQuantity[_8]("solid angle", "")
  object Bit extends BaseQuantity[_9]("bit", "b")

  type *[L <: DList, R <: DList] = L#Mult[R]
  type /[L <: DList, R <: DList] = L#Div[R]

  type Area = Length.Base * Length.Base
  type Volume = Area * Length.Base
  type Density = Volume / Mass.Base
  type Speed = Length.Base / Time.Base
  type Acceleration = Speed / Time.Base
  type Frequency = DNil / Time.Base
  type Force = Mass.Base * Acceleration
  type Pressure = Force / Area
  type Energy = Force * Mass.Base
  type Power = Energy / Time.Base

  object Electric {
    object Current extends BaseQuantity[_5]("electric current", "I")
    type Charge = Current.Base * Time.Base
    type Potential = Power / Current.Base
    type Capacitance = Charge / Potential
    type Resistance = Potential / Current.Base
    type Conductance = Current.Base / Potential
    type Flux = Potential / Time.Base
    type FieldStrength = Flux / Area
    type Inductance = FieldStrength / Current.Base
  }

  type Illuminance = LuminousIntensity.Base / Area

  object Radioactive {
    type Decay = Frequency
    type Dose = Area / (Decay * Decay)
  }

  type CatalyticActivity = AmountOfSubstance.Base / Time.Base
}

object Tests {
  import Quantities._
  import Electric._

  implicitly[Current.Base#Add[Time.Dim] =:= Charge]
  implicitly[Time.Base#Add[Current.Dim] =:= Charge]

  implicitly[Volume#Neg =:= DNel[Dimension[_0,_3#Neg],DNil]]

  implicitly[DNil * DNil =:= DNil]
  implicitly[DNil * Length.Base =:= Length.Base]
  implicitly[Length.Base * DNil =:= Length.Base]
  implicitly[Area =:= DNel[Dimension[_0,_2],DNil]]
  implicitly[Volume =:= DNel[Dimension[_0,_3],DNil]]
  implicitly[Charge =:= DNel[Dimension[_1,_1],DNel[Dimension[_5,_1],DNil]]]
  implicitly[Time.Base * Current.Base =:= Charge]

  implicitly[(DNil / Time.Base) =:= DNel[Dimension[_1,_1#Neg],DNil]]
  implicitly[Speed =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_1#Neg],DNil]]]
  implicitly[Acceleration =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNil]]]
  implicitly[Acceleration * Mass.Base =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_1],DNil]]]]
  // val a: Speed * Mass.Base = 1
  // val b: Mass.Base * Speed = 1
  // implicitly[(Speed * Mass.Base) =:= (Mass.Base * Speed)]

  // Force =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_1],DNil]]].
  // implicitly[(Pressure / DNil) =:= Pressure]
  // val t: Energy / Volume = 1
  // DNel[Dimension[_0,_2#Neg],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_2],DNil]]]
  // implicitly[Energy / Volume =:= Pressure]
}