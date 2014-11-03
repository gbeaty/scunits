package scunits.types

trait Box {
  type of
  type box = BoxOf[of]
  type self <: BoxOf[of]
  type map[F[_ <: of] <: of] = mapTo[of,F]
  type mapTo[B, F[_ <: of] <: B] <: BoxOf[B]
  type flatMap[F[_ <: of] <: BoxOf[of]] = flatMapTo[of,F]
  type flatMapTo[B, F[_ <: of] <: BoxOf[B]] <: BoxOf[B]
  type getOrElse[E <: of] <: of
  type full[C <: of] = Full[of,C]
  type empty = Empty[of]
  type isFull <: Bool
}
trait BoxOf[A] extends Box {
  type of = A
}
trait Full[A,C <: A] extends BoxOf[A] {
  type self = Full[A,C]
  type contents = C
  type mapTo[B, F[_ <: A] <: B] = Full[B,F[C]]
  type flatMapTo[B, F[_ <: A] <: BoxOf[B]] = F[C]
  type getOrElse[E <: A] = C
  type isFull = True
}
trait Empty[A] extends BoxOf[A] {
  type self = Empty[A]
  type mapTo[B, F[_ <: A] <: B] = Empty[B]
  type flatMapTo[B, F[_ <: A] <: BoxOf[B]] = Empty[B]
  type getOrElse[E <: A] = E
  type isFull = False
}

trait BoxIsFull[B <: Box, C <: B#of]