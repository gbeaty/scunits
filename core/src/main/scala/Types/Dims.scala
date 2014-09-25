package scunits.types

import scunits._

trait Dims {
  type Quants <: QList
  type Exps <: DList

  type mult[R <: DimsOf[Quants]] = Quants ^ Exps#mult[R#Exps]
  type div[R <: DimsOf[Quants]] = Quants ^ Exps#mult[R#Exps#neg]
  type neg = Quants ^ Exps#neg
}
trait DimsOf[Q <: QList] extends Dims {
  type Quants = Q
}
trait ^[L <: QList, R <: DList] extends DimsOf[L] {
  type dims = L ^ R
  type Exps = R
}