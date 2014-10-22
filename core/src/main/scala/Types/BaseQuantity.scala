package scunits.types

trait Quant

trait BaseQuantity {
  type of
  type set[L <: Quant, To <: Integer] <: L with of
  type get[L <: of] <: Integer
  type to[E <: Integer] = set[Quant,E]

  class op[L <: of, R <: of, E <: Quant] {
    type getL = get[L]
    type getR = get[R]
    type apply[O[_ <: Integer, _ <: Integer] <: Integer] = set[E, O[getL,getR]]
  }

  type setNonZero[L <: Quant, To <: Integer] = To#isZero#branch[L, L, set[L,To]]
}
trait BaseQuantityOf[Of] extends BaseQuantity {
  type of = Of
}