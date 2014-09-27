package scunits.types

import scunits._

class Converter[F <: QList, T <: QList] {
  type exps[FE <: EList] <: EList
  type Apply[D <: DimsOf[F]] = T ^ exps[D#exps]

  def apply[D <: DimsOf[F]](in: Measure[D]) = Measure[Apply[D]](in.v)
}
class IndexConverter[F <: QList, T <: QList] extends Converter[F,T] {
  type Is <: IList
  type exps[FE <: EList] = Is#ConvertDims[FE]
}
class IndexConverterConst[F <: QList, T <: QList, I <: IList] extends IndexConverter[F,T] {
  type Is = I
}
class QuantFound[Qs <: QList, Q <: BaseQuantity, I <: Integer] {
  type At = I
}

object Converter {
  implicit def quantSearch[Qs <: QNel, Q <: BaseQuantity, I <: Integer](implicit i: QuantFound[Qs#Tail,Q,I]) =
      new QuantFound[Qs,Q,I#succ]
  implicit def quantFound[Qs <: QNelOfHead[Q], Q <: BaseQuantity] = new QuantFound[Qs,Q,i0]

  implicit def indexConverterBuild[F <: QNel, T <: QList, I <: Integer]
    (implicit i: QuantFound[T,F#Head,I], c: IndexConverter[F#Tail,T]) = new IndexConverterConst[F,T,I -: c.Is]

  implicit def indexConverterBuilt[T <: QList] = new IndexConverterConst[QNil,T,INil]
}