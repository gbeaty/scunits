package scunits.types

trait Box[A] {
  type getOrElse[B, F[DA <: A] <: B, E <: B] <: B
  type full[C <: A] = Full[A,C]
  type empty = Empty[A]
  type self = Box[A]
}
trait Full[A,C <: A] extends Box[A] {
  type getOrElse[B, F[DA <: A] <: B, E <: B] = F[C]
}
trait Empty[A] extends Box[A] {
  type getOrElse[B, F[DA <: A] <: B, E <: B] = E
}

object IntBox extends Box[Integer]