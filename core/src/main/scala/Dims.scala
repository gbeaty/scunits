package scunits

import scunits.integer._
import scunits.integer.Ops._

import annotation.implicitNotFound

trait Quantity
trait BaseQuantity[Q <: BaseQuantity[Q]] extends Quantity with ((Q ^ p1) :: Dimless)

trait Dim {
  type Quant <: Quantity
  type Exp <: Integer
  type Inv = Quant ^ Exp#Neg
}
trait DimOf[Q <: Quantity] extends Dim {
  type Quant = Q
}
trait ^[L <: Quantity, R <: Integer] extends DimOf[L] {
  type Exp = R
}

trait Dims {
  type Neg <: Dims
  type Pow[R <: Integer] <: Dims
  type Mult[R <: Integer] <: Dims
  type Set <: Dims
  type Append[R <: Dims] <: Dims
}
trait Dimless extends Dims {
  type Neg = Dimless
  type Pow[R <: Integer] = Dimless
  type Mult[R <: Integer] = Dimless
  type Set = Dimless
  type Append[R <: Dims] = R
}
trait DNel extends Dims {
  type Tail <: Dims
  type Head <: Dim
  type Set = Head with Tail#Set  
}
trait DNelOf[Q <: Quantity] extends DNel {
  type Head <: DimOf[Q]
}
trait ::[L <: Dim, R <: Dims] extends DNelOf[L#Quant] {
  type Tail = R
  type Head = L#Quant ^ L#Exp
  type SetExp[I <: Integer] = (L#Quant ^ I)

  type Neg = SetExp[Head#Exp#Neg] :: Tail#Neg
  type Pow[R <: Integer] = SetExp[Head#Exp#Mult[R]] :: Tail#Pow[R]
  type Mult[R <: Integer] = SetExp[Head#Exp#Add[R]] :: Tail#Mult[R]
  type Append[R <: Dims] = Head :: Tail#Append[R]
}

@implicitNotFound(msg = "Dimensions ${A} and ${B} are not additive")
class Additive[A,B]

class RemoveQuant[Q <: Quantity, In <: Dims] {
  type Exp <: Integer
  type Rem <: Dims
}
class RemovedQuant[Q <: Quantity, In <: Dims, E <: Integer, R <: Dims] extends RemoveQuant[Q,In] {
  type Exp = E
  type Rem = R
}
class Multer[L <: Dims, R <: Dims] { type Out <: Dims }
class Multing[L <: Dims, R <: Dims, O <: Dims] extends Multer[L,R] {
  type Out = O
}