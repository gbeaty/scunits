import scunits._
import scunits.types._

trait LowPriorityImplicits {
  // Converter stuff:
  implicit def indexSearch[Qs <: QNel, Q <: Quantity](implicit i: QuantIndex[Qs#Tail,Q]) =
    new QuantIndex[Qs, Q] { type Index = i.Index#Succ }

  implicit def indexConverterBuild[Q <: Quantity, F <: QNelOfHead[Q], T <: QList]
    (implicit i: QuantIndex[T,Q], c: IndexConverter[F#Tail,T]) =
    new IndexConverterBuilder[F,T,i.Index -: c.Is]
}

package object scunits extends LowPriorityImplicits {  
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv  

  // Converter stuff:
  implicit def indexFound[Qs <: QNelOfHead[Q], Q <: Quantity] = new QuantIndex[Qs,Q] { type Index = i0 }

  implicit def indexConverterBuilt[T <: QList] = new IndexConverterBuilder[QNil,T,INil]

  @annotation.implicitNotFound(msg = "Cannot generate a Converter of type ${From} -> ${To}. Consider creating one manually.")
  def converter[From <: QList, To <: QList](implicit cb: Converter[From,To]) = cb
}