package scunits.types

import scunits._

trait DimsConst[Bs <: Dim, Vs <: Dim] extends Dims {
  type bases = Bs
  type values = Vs
}

trait BaseQuantity {
  type self <: BaseQuantityOf[self]
  type of <: Dim
  type set[E <: Integer] <: of
  type get[L <: of] <: Integer

  type to[E <: NonZeroInt] = DimsConst[self#of, self#set[E]]
  type setDim[L <: Dim, To <: Integer] = L with set[To]
  type dim = to[p1]

  // type setNonZero[L <: Dim, To <: Integer] = To#isZero#branch[L, L, setDim[L,To]]
  type setNonZero[L <: Dim, To <: Integer] = To#ifZero[L, L, ({type nz[I <: NonZeroInt] = setDim[L,I]})#nz]
}
trait BaseQuantityOf[S <: BaseQuantityOf[S]] extends BaseQuantity {
  type self = S
}