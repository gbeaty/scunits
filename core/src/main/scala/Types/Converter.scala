package scunits.types

import scunits._

class Converter[F <: QList, T <: QList] {
  type Exps[FE <: DList] <: DList
  type Apply[D <: DimsOf[F]] = T ^ Exps[D#Exps]
}
class IndexConverter[F <: QList, T <: QList] extends Converter[F,T] {
  type Is <: IList
  type Exps[FE <: DList] = Is#ConvertDims[FE]
}
class IndexConverterConst[F <: QList, T <: QList, I <: IList] extends IndexConverter[F,T] {
  type Is = I
}
class QuantFound[Qs <: QList, Q <: Quantity] {
  type At <: Integer
}
class QuantFoundAt[Qs <: QList, Q <: Quantity, A <: Integer] extends QuantFound[Qs,Q] {
  type At = A
}

object Converter { 
  implicit def quantSearch[Qs <: QNel, Q <: Quantity, I <: Integer](implicit i: QuantFoundAt[Qs#Tail,Q,I#pred]) =
      new QuantFoundAt[Qs,Q,I] 
  implicit def quantFound[Qs <: QNelOfHead[Q], Q <: Quantity] = new QuantFoundAt[Qs,Q,i0]

  implicit def indexConverterBuild[F <: QNel, T <: QList, I <: Integer]
    (implicit i: QuantFoundAt[T,F#Head,I], c: IndexConverter[F#Tail,T]) = new IndexConverterConst[F,T,I -: c.Is]

  implicit def indexConverterBuilt[T <: QList] = new IndexConverterConst[QNil,T,INil]
}