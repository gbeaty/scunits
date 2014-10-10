package scunits.types

trait Box[A] {
  type box = Box[A]
  type self <: Box[A]
  type map[F[_ <: A] <: A] = mapTo[A,F]
  type mapTo[B, F[_ <: A] <: B] <: Box[B]
  type flatMap[F[_ <: A] <: Box[A]] = flatMapTo[A,F]
  type flatMapTo[B, F[_ <: A] <: Box[B]] <: Box[B]
  type getOrElse[E <: A] <: A
  type full[C <: A] = Full[A,C]
  type empty = Empty[A]
  type isFull <: Bool
}
trait Full[A,C <: A] extends Box[A] {
  type self = Full[A,C]
  type contents = C
  type mapTo[B, F[_ <: A] <: B] = Full[B,F[C]]
  type flatMapTo[B, F[_ <: A] <: Box[B]] = F[C]
  type getOrElse[E <: A] = C
  type isFull = True
}
trait Empty[A] extends Box[A] {
  type self = Empty[A]
  type mapTo[B, F[_ <: A] <: B] = Empty[B]
  type flatMapTo[B, F[_ <: A] <: Box[B]] = Empty[B]
  type getOrElse[E <: A] = E
  type isFull = False
}

class Bounded[A] {
  class i[C <: A] {
    type apply = C
  }
}
class Bounded2[A] {
  type apply[C <: A] = C
}
object Bounded {
  type B1 = Bounded[Any]#i[String]#apply
  implicitly[B1 =:= String]

  val b2 = new Bounded2[Integer]

  // type B2 = Bounded2[Integer]#apply[_0]
  type B2 = b2.apply[_0]
}