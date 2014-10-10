package scunits.types

import scunits._
import TestTypes._

object SearchTests {
  def s[Qs <: Quantities, Q <: BaseQuantity, R <: Box[Integer]](qs: Qs, bq: Q)
    (implicit res: QuantSearch[Qs#quants,Q,_0,R]) = res

  // Not founds:
  val nA = s(QNil,A)
  implicitly[nA.res =:= Empty[Integer]]
  val anB = s(Aq,B)
  implicitly[anB.res =:= Empty[Integer]]
  val abcnD = s(ABCq,D)
  implicitly[abcnD.res =:= Empty[Integer]]

  val abcFA = s(ABCq,A)
  implicitly[abcFA.res =:= Full[Integer,_0]]
  val abcFC = s(ABCq,C)
  implicitly[abcFC.res =:= Full[Integer,p2]]
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
    implicit val abToA = converter(ABq,Aq)
    implicit val abToBc = converter(ABq,BCq)
  }
  import TestConverters._

  // Full tests:
  aToAb(Measure[Aq.A](1)): Measure[ABq.A]
  bcToAbcd(Measure[BCq.B#mult[BCq.C]](1)): Measure[ABCDq.B#mult[ABCDq.C]]

  // Partial tests:
  implicitly[abToA.indices =:= (_0 -: Empty[Integer] =: INil)]
  implicitly[abToBc.indices =:= (Empty[Integer] =: _0 -: INil)]

  implicitly[abToA.exps[ABq.B#exps] =:= Empty[EList]]
  implicitly[abToA.exps[ABq.A#exps] =:= Full[EList, p1 *: ENil]]

  abToA(Measure[ABq.A](1)): Measure[Aq.A]
  abToBc(Measure[ABq.B](1)): Measure[BCq.B]

  // Implicit tests:
  (Measure[ABCDq.B](1.0) + Measure[BCq.B](1.0)): Measure[ABCDq.B]
  (Measure[ABCDq.C](1.0) + Measure[BCq.C](1.0)): Measure[ABCDq.C]
}