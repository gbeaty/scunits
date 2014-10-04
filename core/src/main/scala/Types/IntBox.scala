package scunits.types

trait IntBox {
  type doOrElse[B, F[_ <: Integer] <: B, E <: B] <: B
  type getOrElse[E <: Integer] <: Integer
}
trait Full[C <: Integer] extends IntBox {
  type doOrElse[B, F[_ <: Integer] <: B, E <: B] = F[C]
  type getOrElse[E <: Integer] = C
}
trait Empty extends IntBox {
  type doOrElse[B, F[_ <: Integer] <: B, E <: B] = E
  type getOrElse[E <: Integer] = E
}