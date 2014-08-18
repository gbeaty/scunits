package scunits

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

trait Dimensions {
  type Add[R <: DimensionLike] <: DNelLike

  type Mult[R <: Dimensions] = Combine[R]
  type Div[R <: Dimensions] = Combine[R#Neg]

  type Combine[R <: Dimensions] <: Dimensions
  protected type CombineL[L <: Dimensions] <: Dimensions

  type Neg <: Dimensions
}
trait DNelLike extends Dimensions {
  type Head <: DimensionLike
  type Tail <: Dimensions

  type Add[R <: DimensionLike] = (R#Id - Head#Id)#BranchNegZeroPos[
    DNelLike,
    DNel[Dimension[R#Id,R#Mag],DNel[Head,Tail]],
    DNel[Dimension[Head#Id,Head#Mag + R#Mag],Tail],
    DNel[Head,Tail#Add[R]]
  ]

  type Combine[R <: Dimensions] = R#CombineL[DNel[Head,Tail]]
  protected type CombineL[L <: Dimensions] = L#Add[Head]#Combine[Tail]
  type Neg = DNel[Dimension[Head#Id,Head#Mag#Neg],Tail#Neg]
}

trait DNel[H <: DimensionLike, T <: Dimensions] extends DNelLike {
  type Head = H
  type Tail = T
}

trait DNil extends Dimensions {
  type Add[R <: DimensionLike] = DNel[Dimension[R#Id,R#Mag],DNil] 
  type Combine[R <: Dimensions] = R
  protected type CombineL[L <: Dimensions] = L
  type Neg = DNil
}

class BaseQuantity[I <: NonNegInt](val name: String, val symbol: String) {
  type Id = I
  type Base = DNel[Dim, DNil]
  type Dim = Dimension[Id, _1]
}

object Tests {
  import scunits.quantity._
  import Electric._
  import Magnetic._

  implicitly[Current.Base#Add[Time.Dim] =:= Charge]
  implicitly[Time.Base#Add[Current.Dim] =:= Charge]

  implicitly[Volume#Neg =:= DNel[Dimension[_0,_3#Neg],DNil]]

  implicitly[DNil * DNil =:= DNil]
  implicitly[DNil * Length.Base =:= Length.Base]
  implicitly[Length.Base * DNil =:= Length.Base]
  implicitly[Area =:= DNel[Dimension[_0,_2],DNil]]
  implicitly[Volume =:= DNel[Dimension[_0,_3],DNil]]
  implicitly[Charge =:= DNel[Dimension[_1,_1],DNel[Dimension[_8,_1],DNil]]]
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