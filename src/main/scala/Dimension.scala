package scunits

import scunits.integer._
import scunits.integer.Ops._

trait Dimensions {
  type Neg <: Dimensions
  type Mult[Right <: Dimensions] <: Dimensions
  protected type MultL[Left <: DNelLike] <: Dimensions
  type Div[Right <: Dimensions] = Mult[Right#Neg]
}
sealed trait DNelLike extends Dimensions {
  type Base <: BaseQuantityLike
  type Mag <: Integer
  type Tail <: Dimensions
  type Self = DNel[Base,Mag,Tail]

  type Mult[Right <: Dimensions] = Right#MultL[Self]

  protected type MultL[Left <: DNelLike] = (Left#Base#Id - Base#Id)#BranchNegZeroPos[
    Dimensions,
    DNel[Left#Base, Left#Mag,       Left#Tail#Mult[Self]], // Take and inc left.  
    (Left#Mag + Mag)#DimMag[Base,Left#Tail#Mult[Tail]],    // Combine both and inc both.
    DNel[Base,      Mag,            Left#Mult[Tail]]       // Take and inc right.
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