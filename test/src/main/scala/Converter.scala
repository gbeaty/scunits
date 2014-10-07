package scunits.types

import scunits._
import TestTypes._

object SearchTests {
  def s[Qs <: Quantities, Q <: BaseQuantity, R <: Box[Integer]](qs: Qs, bq: Q)
    (implicit res: QuantSearch[Qs#quants,Q,_0,R]) = res

  // Not founds:
  val nA = s(QNil,A)
  implicitly[nA.res =:= IntBox.empty]
  val anB = s(Aq,B)
  implicitly[anB.res =:= IntBox.empty]
  val abcnD = s(ABCq,D)
  implicitly[abcnD.res =:= IntBox.empty]

  val abcFA = s(ABCq,A)
  implicitly[abcFA.res =:= IntBox.full[_0]]
  val abcFC = s(ABCq,C)
  implicitly[abcFC.res =:= IntBox.full[p2]]
}

object ConverterTests {
  object TestConverters {
    // Full:
    implicit val aToAb = converter(Aq,ABq)
    implicit val bToAb = converter(Bq,ABq)
    implicit val bToAbc = converter(Bq,ABCq)
    implicit val cToAbc = converter(Cq,ABCq)
    implicit val bcToAbcd = converter(BCq,ABCDq)

    // Partial:
    implicit val abToA = converter2(ABq,Aq)
    implicit val abToBc = converter2(ABq,BCq)
  }
  import TestConverters._

  // Full tests:
  implicitly[=:[IntBox.full[_0],INil] =:= -:[_0,INil]]
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

  // Partial tests:
  implicitly[abToA.indices =:= (_0 -: IntBox.empty =: INil)]
  implicitly[abToBc.indices =:= (IntBox.empty =: _0 -: INil)]

  implicitly[abToA.apply[ABq.B] =:= Empty[Dims]]
  implicitly[abToA.apply[ABq.A] =:= Full[Dims, An ^ (p1 *: ENil)]]

  // Add/subtract:
  (Measure[ABCDq.B](1.0) + Measure[ABCDq.B](1.0)): Measure[ABCDq.B]
  (Measure[ABCDq.B](1.0) - Measure[ABCDq.B](1.0)): Measure[ABCDq.B]

  (Measure[ABCDq.B](1.0) + bcToAbcd(Measure[BCq.B](1.0))): Measure[ABCDq.B]
  (bcToAbcd(Measure[BCq.B](1.0)) + Measure[ABCDq.B](1.0)): Measure[ABCDq.B]

  (Measure[ABCDq.B](1.0) - bcToAbcd(Measure[BCq.B](1.0))): Measure[ABCDq.B]
  (bcToAbcd(Measure[BCq.B](1.0)) - Measure[ABCDq.B](1.0)): Measure[ABCDq.B]

  // Should not compile:
  // (Measure[ABCDq.B](1.0) + Measure[ABCDq.C](1.0))
  // (Measure[ABCDq.B](1.0) - Measure[ABCDq.C](1.0))
  // (Measure[BCq.B](1.0) + Measure[ABCDq.C](1.0))
  // (Measure[BCq.B](1.0) - Measure[ABCDq.C](1.0))

  // Mult/div:

  // (Measure[ABCDq.B](1.0) * Measure[BCq.C](1.0)): Measure[ABCDq.B#mult[ABCDq.C]]
  // (Measure[BCq.C](1.0) * Measure[ABCDq.B](1.0)): Measure[ABCDq.B#mult[ABCDq.C]]
}