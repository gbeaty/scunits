package scunits

import scunits.types._

sealed trait BaseQuantityLike {
  type Id <: NonNegInt
  type Base = DNelConst[this.type, p1, DNil]
}
class BaseQuantity[I <: NonNegInt](val name: String, val symbol: String) extends BaseQuantityLike {
  type Id = I
}

trait Dims {
  type Neg <: Dims
  type Mult[Right <: Dims] <: Dims
  protected type MultL[Left <: DNel] <: Dims
  type Div[Right <: Dims] = Mult[Right#Neg]
}
trait DNel extends Dims {
  type Base <: BaseQuantityLike
  type Mag <: Integer
  type Tail <: Dims

  type Self = DNelConst[Base,Mag,Tail]
  type Head = Base
}

trait DNelConst[B <: BaseQuantityLike, M <: Integer, T <: Dims] extends DNel {
  override type Head = B
  type Base = B
  type Mag = M
  type Tail = T

  type Mult[Right <: Dims] = Right#MultL[Self]

  protected type MultL[Left <: DNel] = (Left#Base#Id - Base#Id)#BranchNegZeroPos[
    Dims,
    DNelConst[Left#Base, Left#Mag,       Left#Tail#Mult[Self]], // Take and inc left.
    (Left#Mag + Mag)#DimMag[Base,Left#Tail#Mult[Tail]],         // Combine both and inc both.
    DNelConst[Base,      Mag,            Left#Mult[Tail]]       // Take and inc right.
  ]

  type Neg = DNelConst[Base,Mag#Neg,Tail#Neg]
}

trait DNil extends Dims {
  type Neg = DNil
  type Mult[Right <: Dims] = Right
  protected type MultL[Left <: DNel] = Left
}