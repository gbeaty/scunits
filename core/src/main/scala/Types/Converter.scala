package scunits.types

import scunits._

class Converter {
  type from <: QList
  type to <: QList
  type indices <: IList

  type exps[FE <: EList] = indices#ConvertDims[FE]
  type apply[D <: Dims] = to ^ exps[D#exps]
  def apply[D <: DimsOf[from]](in: Measure[D]) = Measure[apply[D]](in.v)
  // def apply[D <: Dims](in: Measure[D]) = Measure[apply[D]](in.v)
}
class ConverterFrom[F <: QList] extends Converter {
  type from = F
}
class ConverterFromTo[F <: QList, T <: QList] extends ConverterFrom[F] {
  type to = T
}
class ConverterConst[F <: QList, T <: QList, I <: IList] extends ConverterFromTo[F,T] {
  type indices = I
}
class QuantFound[Qs <: QList, Q <: BaseQuantity, I <: Integer] {
  type At = I
}
class DimensionOf[Qs <: QList, B <: BaseQuantity, E <: EList] {
  type exps = E
  type dims = Qs ^ E
}

trait CachedConverter