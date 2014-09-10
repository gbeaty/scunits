package scunits.test

import scunits._
import scunits.quantity._
import scunits.si._
import scunits.us._
import scunits.us.Fluid._

import org.specs2.mutable._

class UnitM extends Specification {
  val err = 0.001
  "Units of measurement" should {
    "Apply and unapply measurements" in {
      metre.unapply(metre(1.0)) === 1.0
      mile.unapply(mile(1.0)) === 1.0
      fahrenheit.unapply(fahrenheit(70.0)) must be ~(70.0 +/- err)
    }
    "Compose" in {
      val mpg2 = mile / gallon
      mpg2.unapply(mpg2(1.0)) === 1.0
      mpg2(1.0).v must be ~(mpg(1.0).v +/- err)
      
      val psi2 = poundForce / squareInch
      psi2.unapply(psi2(1.0)) === 1.0
      psi2(1.0).v must be ~(psi(1.0).v +/- err)
    }
  }
}