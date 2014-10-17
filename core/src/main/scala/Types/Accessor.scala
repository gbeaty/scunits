package scunits.types

trait Accessor {
  type self <: Accessor
  type of
  type set[L <: Quant, To <: Integer] <: L with of
  type get[L <: of] <: Integer
  type to[E <: Integer] <: Dims2

  class op[L <: of, R <: of, E <: Quant] {
    type getL = get[L]
    type getR = get[R]
    type apply[O[_ <: Integer, _ <: Integer] <: Integer] = set[E, O[getL,getR]]
  }
}
trait AccessorOf[S <: AccessorOf[S,Of], Of] extends Accessor {
  type self = S
  type of = Of

  type to[E <: Integer] = Dims2Const[self :: ANil, set[Quant,E]]
}