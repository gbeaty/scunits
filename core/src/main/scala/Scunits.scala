import scunits._
import scunits.types._

trait LowPriorityImplicits {
  implicit def identityConverter[Qs <: QList] = new IdentityConverter[Qs] with CachedConverter

  implicit def quantSearch[Qs <: QNel, Q <: BaseQuantity, R <: IntBox, I <: Integer]
    (implicit ts: QuantSearch[Qs#tail,Q,I#succ,R]): QuantSearch[Qs,Q,I,R] = null
}

package object scunits extends LowPriorityImplicits {
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv

  implicit def convert[C <: ConverterFrom[D#quants] with CachedConverter, D <: Dims](m: Measure[D])(implicit c: C) =
    Measure[c.apply[D]](m.v)
  
  def converter[F <: Quantities, T <: Quantities, Is <: IList](f: F, t: T)
    (implicit c: IndicesConverter[F#quants,T#quants,Is]) =
      new IndicesConverter[F#quants,T#quants,Is] with CachedConverter
  
  implicit def quantFound[Qs <: QNelOf[Q], Q <: BaseQuantity, I <: Integer]:
    QuantSearch[Qs,Q,I,Full[I]] = null
  implicit def quantNotfound[Q <: BaseQuantity, I <: Integer]:
    QuantSearch[QNil,Q,I,Empty] = null

  implicit def indexConverterBuild[F <: QNel, T <: QList, R <: IntBox, Is <: IList]
    (implicit i: QuantSearch[T,F#head,_0,R], c: IndicesConverter[F#tail,T,Is]): IndicesConverter[F,T,R =: Is] = null

  implicit def indexConverterBuilt[T <: QList]: IndicesConverter[QNil,T,INil] = null

  implicit def dimOfSearch[Qs <: QNel, B <: BaseQuantity, E <: EList](implicit d: DimensionOf[Qs#tail,B,E]):
    DimensionOf[Qs,B,_0 *: E] = null
  implicit def dimOfFound[Qs <: QNelOf[B], B <: BaseQuantity]: DimensionOf[Qs,B,p1 *: ENil] = null
}