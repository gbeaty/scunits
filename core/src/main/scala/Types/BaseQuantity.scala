package scunits.types

import scunits._

trait DimsConst[Bs, Vs] extends Dims {
  type bases = Bs
  type values = Vs
}

trait BaseQuantity {
  type self <: BaseQuantityOf[self]
  type of
  type set[E <: Integer] <: of
  type get[L <: of] <: Integer

  type to[E <: NonZeroInt] = DimsConst[self#of, self#set[E]]
  type dim = to[p1]

  type setNonZero[L, To <: Integer] = To#ifZero[L, L, ({type nz[I <: NonZeroInt] = L with set[I]})#nz]
}
trait BaseQuantityOf[S <: BaseQuantityOf[S]] extends BaseQuantity {
  type self = S
}