package scunits.types

trait Dims

trait BaseQuantity {
  type of
  type set[E <: Integer] <: of
  type get[L <: of] <: Integer

  type to[E <: Integer] = Dims with set[E]
  type setDim[L <: Dims, To <: Integer] = L with set[To]
  type dim = to[p1]
  
  class op[L <: of, R <: of, E <: Dims] {
    type getL = get[L]
    type getR = get[R]
    type apply[O[_ <: Integer, _ <: Integer] <: Integer] = setDim[E, O[getL,getR]]
  }

  type setNonZero[L <: Dims, To <: Integer] = To#isZero#branch[L, L, setDim[L,To]]
}