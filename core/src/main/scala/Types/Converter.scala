package scunits.types

import scunits._

class Converter {
  type from <: QList
  type to <: QList  

  type exps[FE <: EList] <: EList
  type apply[D <: Dims] <: DimsOf[to]
  def apply[D <: DimsOf[from]](in: Measure[D]) = Measure[apply[D]](in.v)
}
class ConverterFrom[F <: QList] extends Converter {
  type from = F
}
class ConverterFromTo[F <: QList, T <: QList] extends ConverterFrom[F] {
  type to = T
}
class IndicesConverter[F <: QList, T <: QList, I <: IList] extends ConverterFromTo[F,T] {
  type indices = I
  type exps[FE <: EList] = indices#ConvertDims[FE]
  type apply[D <: Dims] = to ^ exps[D#exps]
}
class IdentityConverter[Qs <: QList] extends ConverterFromTo[Qs,Qs] {
  type exps[FE <: EList] = FE
}

class QuantFound[Qs <: QList, Q <: BaseQuantity, I <: Integer] {
  type At = I
}
class DimensionOf[Qs <: QList, B <: BaseQuantity, E <: EList] {
  type exps = E
  type dims = Qs ^ E
}

trait CachedConverter