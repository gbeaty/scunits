package scunits.types

import scunits._
import TestTypes._

object SearchTests {
  def has[Qs <: Quantities, Q <: BaseQuantity, I <: Integer](in: Qs, find: Q)(implicit i: QuantFound[Qs#quants,Q,I]) = i

  val a = has(ABCDq,A)
  implicitly[a.at =:= _0]

  val b = has(ABCDq,B)
  implicitly[b.at =:= p1]

  val c = has(ABCDq,C)
  implicitly[c.at =:= p2]

  val d = has(ABCDq,D)
  implicitly[d.at =:= p3]
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

  implicitly[aToAb.indices =:= (_0 -: INil)]
  implicitly[aToAb.apply[Aq.Dimless] =:= ABq.Dimless]

  implicitly[bToAb.indices =:= (p1 -: INil)]
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
  implicitly[aDim.dims =:= (ABCDn ^ (p1 *: ENil))]

  val bDim = ABCDq.dimOf(B)
  implicitly[bDim.dims =:= (ABCDn ^ (_0 *: p1 *: ENil))]

  val cDim = ABCDq.dimOf(C)
  implicitly[cDim.dims =:= (ABCDn ^ (_0 *: _0 *: p1 *: ENil))]
}

object MeasureConverterTests {
  // implicit val aToAbcd = converter(Aq,ABCDq)
  // implicit val abToAbcd = converter(ABq,ABCDq)
  // implicit val abcToAbcd = converter(ABCq,ABCDq)
  // implicit val abdToAbcd = converter(ABDq,ABCDq)
  // implicit val bToAbcd = converter(Bq,ABCDq)
  // implicit val cToAbcd = converter(Cq,ABCDq)
  implicit val bcToAbcd = converter(BCq,ABCDq)
  // implicit val cdToAbcd = converter(CDq,ABCDq)

  // Add/subtract:
  (Measure[ABCDq.B](1.0) + Measure[ABCDq.B](1.0)): Measure[ABCDq.B]
  (Measure[ABCDq.B](1.0) - Measure[ABCDq.B](1.0)): Measure[ABCDq.B]

  (Measure[ABCDq.B](1.0) + Measure[BCq.B](1.0)): Measure[ABCDq.B]
  (Measure[BCq.B](1.0) + Measure[ABCDq.B](1.0)): Measure[ABCDq.B]

  (Measure[ABCDq.B](1.0) - Measure[BCq.B](1.0)): Measure[ABCDq.B]
  (Measure[BCq.B](1.0) - Measure[ABCDq.B](1.0)): Measure[ABCDq.B]

  // Should not compile:
  // (Measure[ABCDq.B](1.0) + Measure[ABCDq.C](1.0))
  // (Measure[ABCDq.B](1.0) - Measure[ABCDq.C](1.0))
  // (Measure[BCq.B](1.0) + Measure[ABCDq.C](1.0))
  // (Measure[BCq.B](1.0) - Measure[ABCDq.C](1.0))

  // Mult/div:

  // (Measure[ABCDq.B](1.0) * Measure[BCq.C](1.0)): Measure[ABCDq.B#mult[ABCDq.C]]
  // (Measure[BCq.C](1.0) * Measure[ABCDq.B](1.0)): Measure[ABCDq.B#mult[ABCDq.C]]

  // ConverterFromTo[::[A.type,::[B.type,::[C.type,::[D.type,QNil]]]],BCq.C#quants] with CachedConverter
}
