package scunits.types

import scunits._

class Converter[F <: QList, T <: QList] {
  type exps[FE <: EList] <: EList
  type Apply[D <: DimsOf[F]] = T ^ exps[D#exps]
}
class IndexConverter[F <: QList, T <: QList] extends Converter[F,T] {
  type Is <: IList
  type exps[FE <: EList] = Is#ConvertDims[FE]
}
class IndexConverterConst[F <: QList, T <: QList, I <: IList] extends IndexConverter[F,T] {
  type Is = I
}
class QuantFound[Qs <: QList, Q <: BaseQuantity] {
  type At <: Integer
}
class QuantFoundAt[Qs <: QList, Q <: BaseQuantity, A <: Integer] extends QuantFound[Qs,Q] {
  type At = A
}

object Converter { 
  implicit def quantSearch[Qs <: QNel, Q <: BaseQuantity, I <: Integer](implicit i: QuantFoundAt[Qs#Tail,Q,I#pred]) =
      new QuantFoundAt[Qs,Q,I] 
  implicit def quantFound[Qs <: QNelOfHead[Q], Q <: BaseQuantity] = new QuantFoundAt[Qs,Q,i0]

  implicit def indexConverterBuild[F <: QNel, T <: QList, I <: Integer]
    (implicit i: QuantFoundAt[T,F#Head,I], c: IndexConverter[F#Tail,T]) = new IndexConverterConst[F,T,I -: c.Is]

  implicit def indexConverterBuilt[T <: QList] = new IndexConverterConst[QNil,T,INil]
}