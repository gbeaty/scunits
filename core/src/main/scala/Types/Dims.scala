package scunits.types

import scunits._

trait Dims {
  type quants <: QList
  type exps <: EList

  type mult[R <: DimsOf[quants]] = quants ^ exps#mult[R#exps]
  type div[R <: DimsOf[quants]] = quants ^ exps#mult[R#exps#neg]
  type neg = quants ^ exps#neg
}
trait DimsOf[Q <: QList] extends Dims {
  type quants = Q
}
trait ^[L <: QList, R <: EList] extends DimsOf[L] {
  type dims = L ^ R
  type exps = R
}