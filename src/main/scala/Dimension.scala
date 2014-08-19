package scunits

import scunits.integer._
import scunits.integer.Ops._

trait Dimensions {
  type Add[R <: DNelLike] <: Dimensions

  type Mult[R <: Dimensions] = Combine[R]
  type Div[R <: Dimensions] = Combine[R#Neg]

  type Combine[R <: Dimensions] <: Dimensions
  protected type CombineL[L <: DNelLike] <: Dimensions

  type Neg <: Dimensions
}
trait DNelLike extends Dimensions {
  type Base <: BaseQuantityLike
  type Mag <: Integer
  type Tail <: Dimensions
  type Self = DNel[Base,Mag,Tail]
  type NewMag[M <: Integer] = DNel[Base,M,Tail]

  type Add[R <: DNelLike] = (Mag+R#Mag)#DimMag[Base,Tail]
  /*type Add[R <: DNelLike] = (R#Base#Id - Base#Id)#BranchNegZeroPos[
    Dimensions,
    DNel[R#Base,R#Mag,this.type],
    // (Mag+R#Mag)#BranchZero[Dimensions,Tail,NewMag],
    // DNel[Base,Mag + R#Mag,Tail],
    (Mag+R#Mag)#DimMag[this.type],
    DNel[Base,Mag,Tail#Add[R]]
  ]*/

  type Combine[R <: Dimensions] = R#CombineL[this.type]
  protected type CombineL[L <: DNelLike] = L#Add[this.type]#Combine[Tail]
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
  protected type CombineL[L <: DNelLike] = L
  type Neg = DNil
}

object Tests {
  import scunits.quantity._
  import Electric._
  import Magnetic._

  // implicitly[Length * DNil =:= Length]
  // implicitly[DNil * Length =:= Length]

  // implicitly[Length#Add[Length#Neg] =:= DNil]
  // val t: Length#Add[Length] = 1
  // implicitly[Length#Add[Length] =:= DNel[Length.type,_2,DNil]]

  // Length#Mag#Succ#DimMag[_ <: DNel[Length.type,_1,DNil] with Singleton]

  /*
  Length#Base#Id#Sub[_0]#BranchNegZeroPos[
    Dimensions,
    DNel[Length#Base,Length#Mag,DNel[Length.type,_1,DNil]],
    Length#Mag#Succ#DimMag[DNel[Length.type,_1,DNil]],
    DNel[Length.type,SuccInt[_0],DNel[Length#Base,Length#Mag,DNil]]
  ]
  */

  /*Length#Base#Id#Sub[_0]#BranchNegZeroPos[
    Dimensions,
    DNel[Length#Base,Length#Mag,DNel[Length.type,_1,DNil]],
    Length#Mag#Succ#BranchZero[
      Dimensions,
      DNil,
      [M <: Integer]DNel[Length.type,M,DNil]
    ],
    DNel[Length.type,SuccInt[_0],DNel[Length#Base,Length#Mag,DNil]]
  ]*/

  // implicitly[Length#Add[Time#Neg] =:= DNel[Length.type,_1,DNel[Time.type,_1#Neg,DNil]]]

  // implicitly[Area =:= DNel[Length.type,_2,DNil]]
  // Length#Combine[Length] = Length#CombineL[Length] = Length#Add[Length]#Combine[Length#Tail] =
  // Length#Add[Length]#Combine[DNil] = Area#Combine[DNil] = DNil#CombineL[Area] = Area
  
  /*
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
  */

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