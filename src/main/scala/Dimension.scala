package scunits

import scunits.integer._
import scunits.integer.Ops._

trait Dimensions {
  type Add[R <: DNelLike] <: DNelLike

  type Mult[R <: Dimensions] = Combine[R]
  type Div[R <: Dimensions] = Combine[R#Neg]

  type Combine[R <: Dimensions] <: Dimensions
  protected type CombineL[L <: Dimensions] <: Dimensions

  type Neg <: Dimensions
}
trait DNelLike extends Dimensions {
  type Base <: BaseQuantityLike
  type Mag <: Integer
  type Tail <: Dimensions

  type Add[R <: DNelLike] = (R#Base#Id - Base#Id)#BranchNegZeroPos[
    DNelLike,
    DNel[R#Base,R#Mag,DNel[Base,Mag,Tail]],
    DNel[Base,Mag + R#Mag,Tail],
    DNel[Base,Mag,Tail#Add[R]]
  ]

  type Combine[R <: Dimensions] = R#CombineL[DNel[Base,Mag,Tail]]
  protected type CombineL[L <: Dimensions] = L#Add[DNel[Base,Mag,Tail]]#Combine[Tail]
  type Neg = DNel[Base,Mag#Neg,Tail#Neg]
}

trait DNel[B <: BaseQuantityLike, M <: Integer, T <: Dimensions] extends DNelLike {
  type Base = B
  type Mag = M
  type Tail = T
}

trait DNil extends Dimensions {
  type Add[R <: DNelLike] = DNel[R#Base,R#Mag,DNil] 
  type Combine[R <: Dimensions] = R
  protected type CombineL[L <: Dimensions] = L
  type Neg = DNil
}

object Tests {
  import scunits.quantity._
  import Electric._
  import Magnetic._

  // DNel[Length.type,_3#Neg,DNil]
  // DNel[Length,     _3#Neg,DNil]
  implicitly[Volume#Neg =:= DNel[Length.type,_3#Neg,DNil]]

  implicitly[DNil * DNil =:= DNil]
  implicitly[DNil * Length =:= Length]
  implicitly[Length * DNil =:= Length]
  implicitly[Area =:= DNel[Length.type,  _2,DNil]]
  implicitly[Volume =:= DNel[Length.type,_3,DNil]]
  implicitly[Charge =:= DNel[Time.type,  _1,DNel[Current.type,_1,DNil]]]
  implicitly[Time * Current =:= Charge]

  implicitly[(DNil / Time) =:= DNel[Time.type,_1#Neg,DNil]]
  implicitly[Speed =:= DNel[Length.type,_1,DNel[Time.type,_1#Neg,DNil]]]
  implicitly[Acceleration =:= DNel[Length.type,_1,DNel[Time.type,_2#Neg,DNil]]]
  implicitly[Acceleration * Mass =:= DNel[Length.type,_1,DNel[Time.type,_2#Neg,DNel[Mass.type,_1,DNil]]]]

  // implicitly[Length / Length =:= DNil]

  // implicitly[Mass * Acceleration =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_1],DNil]]]]
  // val t: (Mass * Acceleration) = 5
  // DNel[_28.Head,_28.Tail] forSome { val _28: DNel[Dimension[_0,_1],DNel[Dimension[_2,_1],DNil]]; val _28: DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg,DNel[Dimension[_2,_1],DNil]]] }
  // val a: Speed * Mass = 1
  // val b: Mass * Speed = 1
  // implicitly[(Speed * Mass) =:= (Mass * Speed)]

  // Force =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_1],DNil]]].
  // implicitly[(Pressure / DNil) =:= Pressure]
  // val t: Energy / Volume = 1
  // DNel[Dimension[_0,_2#Neg],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_2],DNil]]]
  // implicitly[Energy / Volume =:= Pressure]
}