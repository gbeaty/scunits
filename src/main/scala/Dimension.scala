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
  type Id <: NonNegInt
  type Mag <: Integer
  type Tail <: Dimensions
  type Self = DNel[Id,Mag,Tail]

  type Mult[Right <: Dimensions] = Right#MultL[Self]

  protected type MultL[Left <: DNelLike] = (Left#Id - Id)#BranchNegZeroPos[
    Dimensions,
    DNel[Left#Id, Left#Mag,       Left#Tail#Mult[Self]], // Take and inc left.  
    (Left#Mag + Mag)#DimMag[Id,Left#Tail#Mult[Tail]],    // Combine both and inc both.
    DNel[Id,      Mag,            Left#Mult[Tail]]       // Take and inc right.
  ]

  type Neg = DNel[Id,Mag#Neg,Tail#Neg]
}

trait DNel[I <: NonNegInt, M <: Integer, T <: Dimensions] extends DNelLike {
  type Id = I
  type Mag = M
  type Tail = T
}

trait DNil extends Dimensions {
  type Neg = DNil
  type Mult[Right <: Dimensions] = Right
  protected type MultL[Left <: DNelLike] = Left
}

object Test {
  type *[L <: Dimensions, R <: Dimensions] = L#Mult[R]
  type /[L <: Dimensions, R <: Dimensions] = L#Div[R]

  type Test = DNel[_0,_1,DNel[_1,_1#Neg,DNil]]#Mult[DNel[_0,_1,DNil]]
  type Test2 = DNel[_0,_1,DNil]#Mult[DNel[_0,_1,DNel[_1,_1#Neg,DNil]]]
}