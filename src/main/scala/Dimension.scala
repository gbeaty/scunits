package scunits

import scunits.integer._
import scunits.integer.Ops._

trait Dimension {
  type Base <: BaseQuantityLike
  type Mag <: Integer
  type Append[T <: Dimensions] = DNel[Base,Mag,T]
}
trait DimConst[B <: BaseQuantityLike, M <: Integer] extends Dimension {
  type Base = B
  type Mag = M
}
trait Dimensions {
  type Neg <: Dimensions
  type Mult[Right <: Dimensions] <: Dimensions
  protected type MultL[Left <: DNelLike] <: Dimensions
  type Div[Right <: Dimensions] = Mult[Right#Neg]
}
trait DNelLike extends Dimensions with Dimension {
  type Base <: BaseQuantityLike
  type Mag <: Integer
  type Tail <: Dimensions
  type Self = DNel[Base,Mag,Tail]

  type Mult[Right <: Dimensions] = Right#MultL[Self]

  protected type MultL[Left <: DNelLike] = (Left#Base#Id - Base#Id)#BranchNegZeroPos[
    Dimensions,
    DNel[Left#Base,       Left#Mag,       Left#Tail#Mult[Self]], // Take and inc left.
    DNel[Left#Base,       Left#Mag + Mag, Left#Tail#Mult[Tail]], // Combine both and inc both.
    DNel[Base,            Mag,            Left#Mult[Tail]]       // Take and inc right.
  ]

  type Neg = DNel[Base,Mag#Neg,Tail#Neg]
}

trait DNel[B <: BaseQuantityLike, M <: Integer, T <: Dimensions] extends DNelLike {
  type Base = B
  type Mag = M
  type Tail = T  
}

trait DNil extends Dimensions {
  type Neg = DNil
  type Mult[Right <: Dimensions] = Right
  protected type MultL[Left <: DNelLike] = Left
}

object Tests {
  import scunits.quantity._

  type ::[L <: Dimension, R <: Dimensions] = L#Append[R]

  /*implicitly[Length#Neg =:= (Length.Mag[_1#Neg] :: DNil)]
  implicitly[Length * DNil =:= Length]
  implicitly[DNil * Length =:= Length]
  implicitly[Area =:= (Length.Mag[_2] :: DNil)]
  implicitly[Volume =:= (Length.Mag[_3] :: DNil)]

  implicitly[Length / Time =:= (Length.Mag[_1] :: Time.Mag[_1#Neg] :: DNil)]
  // implicitly[Electric.Charge =:= (Time.Mag[_1] :: Electric.Current.Mag[_1] :: DNil)]  

  implicitly[(DNil / Time) =:= (Time.Mag[_1#Neg] :: DNil)]
  implicitly[Speed =:= (Length.Mag[_1] :: Time.Mag[_1#Neg] :: DNil)]
  implicitly[Acceleration =:= (Length.Mag[_1] :: Time.Mag[_2#Neg] :: DNil)]
  implicitly[Acceleration * Mass =:= (Length.Mag[_1] :: Time.Mag[_2#Neg] :: Mass.Mag[_1] :: DNil)]*/

  // val a: (Speed * Mass) = 1
  // val a: (Mass * Speed) = 1
  // implicitly[(Speed * Mass) =:= (Mass * Speed)]

  // implicitly[Force =:= (Length.Mag[_1] :: Time.Mag[_2#Neg] :: Mass.Mag[_1])]

  // implicitly[Energy / Volume =:= Pressure]
}