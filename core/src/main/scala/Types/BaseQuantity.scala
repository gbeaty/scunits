package scunits.types

trait Quant

trait BaseQuantity {
  type of
  type set[E <: Integer] <: of
  type get[L <: of] <: Integer

  type to[E <: Integer] = Quant with set[E]
  type setQuant[L <: Quant, To <: Integer] = L with set[To]
  
  class op[L <: of, R <: of, E <: Quant] {
    type getL = get[L]
    type getR = get[R]
    type apply[O[_ <: Integer, _ <: Integer] <: Integer] = setQuant[E, O[getL,getR]]
  }

  type setNonZero[L <: Quant, To <: Integer] = To#isZero#branch[L, L, setQuant[L,To]]
}