package scunits.types

import scunits._

trait Dims {
  type quants <: QList
  type exps <: EList

  type mult[R <: Dims] <: DimsOf[quants]
  type div[R <: Dims] <: DimsOf[quants]
  type neg <: Dims
}
trait DimsOf[Q <: QList] extends Dims {
  type quants = Q
}
trait ^[L <: QList, R <: EList] extends DimsOf[L] {
  type exps = R
  
  type mult[R <: Dims] = quants ^ exps#mult[R#exps]
  type div[R <: Dims] = quants ^ exps#mult[R#exps#neg]
  type neg = quants ^ exps#neg
}