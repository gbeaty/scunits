package scunits.types

import scunits._

class Converter[F <: QList, T <: QList] {
  type Exps[FE <: DList] <: DList
}
class IndexConverter[F <: QList, T <: QList] {
  type Is <: Indexes
  type Exps[FE <: DList] = Is#ConvertDims[FE]
}
class IndexConverterBuilder[F <: QList, T <: QList, I <: Indexes] extends Converter[F,T] {
  type Is = I
}

class QuantIndex[A <: QList, Q <: Quantity] {
  type Index <: NonNegInt
}