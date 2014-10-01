package scunits.types

import scunits._
import TestTypes._

object SearchTests {
  def has[Qs <: Quantities, Q <: BaseQuantity, I <: Integer](in: Qs, find: Q)(implicit i: QuantFound[Qs#quants,Q,I]) = i

  val a = has(ABCDq,A)
  implicitly[a.at =:= i0]

  val b = has(ABCDq,B)
  implicitly[b.at =:= i1]

  val c = has(ABCDq,C)
  implicitly[c.at =:= i2]

  val d = has(ABCDq,D)
  implicitly[d.at =:= i3]
}

object ConverterTests {
  object TestConverters {
    implicit val aToAb = converter(Aq,ABq)
    implicit val bToAb = converter(Bq,ABq)
    implicit val bToAbc = converter(Bq,ABCq)
    implicit val cToAbc = converter(Cq,ABCq)
    implicit val bcToAbcd = converter(BCq,ABCDq)
  }
  import TestConverters._

  implicitly[aToAb.indices =:= (i0 -: INil)]
  implicitly[aToAb.apply[Aq.Dimless] =:= ABq.Dimless]

  implicitly[bToAb.indices =:= (i1 -: INil)]
  implicitly[bToAb.apply[Bq.Dimless] =:= ABq.Dimless]
  implicitly[bToAb.apply[Bq.B] =:= ABq.B]

  val aOne = Measure[Aq.A](1.0)
  val abOne = Measure[ABq.A](1.0)
  val bcOne = Measure[BCq.B#mult[BCq.C]](1.0)

  Measure[aToAb.apply[Aq.A]](1.0) === abOne
  aToAb(aOne) === abOne
  bcToAbcd(bcOne) === Measure[ABCDq.B#mult[ABCDq.C]](1.0)
}

object DimOfTests {
  val aDim = ABCDq.dimOf(A)
  implicitly[aDim.dims =:= (ABCDn ^ (i1 *: ENil))]

  val bDim = ABCDq.dimOf(B)
  implicitly[bDim.dims =:= (ABCDn ^ (i0 *: i1 *: ENil))]

  val cDim = ABCDq.dimOf(C)
  implicitly[cDim.dims =:= (ABCDn ^ (i0 *: i0 *: i1 *: ENil))]
}