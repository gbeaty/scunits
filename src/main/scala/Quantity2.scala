package scunits2

import scunits.tlist2._
import scunits.integer._
import scunits.integer.Ops._

trait DimensionLike {
  type Id <: NonNegInt
  type Mag <: Integer
}
trait Dimension[I <: NonNegInt, M <: Integer] extends DimensionLike {
  type Mag = M 
  type Id = I
}

trait DList {
  type Add[R <: DimensionLike] = Op[R,+]
  type Sub[R <: DimensionLike] = Op[R,-]
  type Op[R <: DimensionLike, O[_ <: Integer, _ <: Integer] <: Integer] <: DNelLike

  type Mult[R <: DList] = Combine[R,+]
  type Div[R <: DList] = Combine[R,-]

  type Combine[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] <: DList
  protected type CombineL[L <: DList, O[_ <: Integer, _ <: Integer] <: Integer] <: DList

  type Negate <: DList
}
trait DNelLike extends DList {
  type Head <: DimensionLike
  type Tail <: DList

  type Op[R <: DimensionLike, O[_ <: Integer, _ <: Integer] <: Integer] = (R#Id - Head#Id)#BranchNegZeroPos[
    DNelLike,
    DNel[Dimension[R#Id,O[_0,R#Mag]],DNel[Head,Tail]],
    DNel[Dimension[Head#Id,O[Head#Mag,R#Mag]],Tail],
    DNel[Head,Tail#Op[R,O]]
  ]

  type Combine[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = R#CombineL[DNel[Head,Tail],O]
  protected type CombineL[L <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = L#Op[Head,O]#Combine[Tail,O]
  type Negate = DNel[Dimension[Head#Id,Head#Mag#Neg],Tail#Negate]
}

trait DNel[H <: DimensionLike, T <: DList] extends DNelLike {
  type Head = H
  type Tail = T
}

trait DNil extends DList {
  type Op[R <: DimensionLike, O[_ <: Integer, _ <: Integer] <: Integer] = DNel[Dimension[R#Id,O[_0,R#Mag]],DNil]
  type Combine[R <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = R
  protected type CombineL[L <: DList, O[_ <: Integer, _ <: Integer] <: Integer] = L
  type Negate = DNil
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

  implicitly[Volume#Negate =:= DNel[Dimension[_0,_3#Neg],DNil]]

  implicitly[DNil * DNil =:= DNil]
  implicitly[DNil * Length.Base =:= Length.Base]
  implicitly[Length.Base * DNil =:= Length.Base]
  implicitly[Area =:= DNel[Dimension[_0,_2],DNil]]
  implicitly[Volume =:= DNel[Dimension[_0,_3],DNil]]
  implicitly[Charge =:= DNel[Dimension[_1,_1],DNel[Dimension[_5,_1],DNil]]]
  implicitly[Time.Base * Current.Base =:= Charge]

  // implicitly[(DNil / Time.Base) =:= DNel[Dimension[_1,_1#Neg],DNil]]
}