import scunits._
import scunits.types._

trait LowPriorityImplicits {
  implicit def identityConverter[Qs <: QList] = new IdentityConverter[Qs] with CachedConverter
}

package object scunits extends LowPriorityImplicits {
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv

  implicit def convert[C <: ConverterFrom[D#quants] with CachedConverter, D <: Dims](m: Measure[D])(implicit c: C) =
    Measure[c.apply[D]](m.v)

  @annotation.implicitNotFound(msg = "Cannot generate a Converter of type ${F} -> ${T}. Consider creating one manually.")
  def converter[F <: Quantities, T <: Quantities, Is <: IList](f: F, t: T)
    (implicit c: IndicesConverter[F#quants,T#quants,Is]) =
      new IndicesConverter[F#quants,T#quants,Is] with CachedConverter

  implicit def quantSearch[Qs <: QNel, Q <: BaseQuantity, I <: Integer](implicit i: QuantFound[Qs#tail,Q,I]) =
      new QuantFound[Qs,Q,I#succ]
  implicit def quantFound[Qs <: QNelOfHead[Q], Q <: BaseQuantity] = new QuantFound[Qs,Q,i0]

  implicit def indexConverterBuild[F <: QNel, T <: QList, I <: Integer, Is <: IList]
    (implicit i: QuantFound[T,F#head,I], c: IndicesConverter[F#tail,T,Is]) = new IndicesConverter[F,T,I -: Is]

  implicit def indexConverterBuilt[T <: QList] = new IndicesConverter[QNil,T,INil]

  implicit def dimOfSearch[Qs <: QNel, B <: BaseQuantity, E <: EList](implicit d: DimensionOf[Qs#tail,B,E]) =
    new DimensionOf[Qs,B,i0 *: E]
  implicit def dimOfFound[Qs <: QNelOfHead[B], B <: BaseQuantity] = new DimensionOf[Qs,B,i1 *: ENil]
}