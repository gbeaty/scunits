package scunits.types

import scunits._

class Converter[F <: QList, T <: QList] {
  type Exps[FE <: DList] <: DList
  type Apply[D <: Dims] = T ^ Exps[D#Exps]
}
class IndexConverter[F <: QList, T <: QList] extends Converter[F,T] {
  type Is <: Indexes
  type Exps[FE <: DList] = Is#ConvertDims[FE]
}
class IndexConverterBuilder[F <: QList, T <: QList, I <: Indexes] extends IndexConverter[F,T] {
  type Is = I
}

class QuantIndex[A <: QList, Q <: Quantity] {
  type Index <: NonNegInt
}
class QuantIndexConst[A <: QList, Q <: Quantity, I <: NonNegInt] extends QuantIndex[A,Q] {
  type Index = I
}

object Converter {  
  implicit def indexSearch[Qs <: QNel, Q <: Quantity, I <: NonNegInt](implicit i: QuantIndexConst[Qs#Tail,Q,I]) =
    new QuantIndexConst[Qs, Q, I#Succ]
  implicit def indexFound[Qs <: QNelOfHead[Q], Q <: Quantity] = new QuantIndexConst[Qs,Q,i0]

  implicit def indexConverterBuild[Q <: Quantity, F <: QNelOfHead[Q], T <: QList, I <: NonNegInt]
    (implicit i: QuantIndexConst[T,Q,I], c: IndexConverter[F#Tail,T]) =
    new IndexConverterBuilder[F,T,I -: c.Is]

  implicit def indexConverterBuilt[T <: QList] = new IndexConverterBuilder[QNil,T,INil]    
}