package scunits.types

trait Box[A] {
  type box = Box[A]
  type self <: Box[A]
  type map[F[_ <: A] <: A] = mapTo[A,F]
  type mapTo[B, F[_ <: A] <: B] <: Box[B]
  type flatMap[F[_ <: Box[A]] <: Box[A]] = flatMapTo[A,F]
  type flatMapTo[B, F[_ <: Box[A]] <: Box[B]] <: Box[B]
  type getOrElse[B, F[DA <: A] <: B, E <: B] <: B
  type full[C <: A] = Full[A,C]
  type empty = Empty[A]
}
trait Full[A,C <: A] extends Box[A] {
  type self = Full[A,C]
  type contents = C
  type mapTo[B, F[_ <: A] <: B] = Full[B,F[C]]
  type flatMapTo[B, F[_ <: Box[A]] <: Box[B]] = F[self]
  type getOrElse[B, F[DA <: A] <: B, E <: B] = F[C]
}
trait Empty[A] extends Box[A] {
  type self = Empty[A]
  type mapTo[B, F[_ <: A] <: B] = Empty[B]
  type flatMapTo[B, F[_ <: Box[A]] <: Box[B]] = Empty[B]
  type getOrElse[B, F[DA <: A] <: B, E <: B] = E
}

trait IsFull[B <: DBox.box] {
  type contents <: Dims
}

object IntBox extends Box[Integer]
object EBox extends Box[EList]
object DBox extends Box[Dims]