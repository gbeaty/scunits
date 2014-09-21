package scunits.types

import scunits._

trait Dims {
  type Quants <: QList
  type Exps <: DList

  type Mult[R <: Dims] <: DimsOf[Quants]
  type Div[R <: Dims] <: DimsOf[Quants]
  type Neg <: DimsOf[Quants]
}
trait DimsOf[Q <: QList] extends Dims {
  type Quants = Q
}
trait ^[L <: QList, R <: DList] extends DimsOf[L] {
  type Exps = R

  type Mult[R <: Dims] = Quants ^ Exps#Mult[R#Exps]
  type Div[R <: Dims] = Quants ^ Exps#Mult[R#Exps#Neg]
  type Neg = Quants ^ Exps#Neg
}