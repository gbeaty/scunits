package scunits

import scunits.types._
import scunits.si._

object ScalarTests {
  trait BQ extends BaseQuantityOf[BQ] {
    trait of { type length <: Integer }
    type set[To <: Integer] = of { type length = To }
    type get[L <: of] = L#length
  }
  val bu = UnitM[BQ#dim]("","",1)

  metre(2) * metre(3): Scalar[Area]

  // Should not compile:
  // metre(2) * bu(1)
}