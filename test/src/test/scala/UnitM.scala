package scunits.test

import scunits._
import scunits.default._
import scunits.si._
import scunits.us._
import scunits.us.Fluid._

import org.specs2.mutable._

class UnitMTests extends Specification {
  val err = 0.001
  "Units of measurement" should {
    "Apply and unapply measurements" in {
      metre(1.0).to(metre) === 1.0
      mile(1.0).to(mile) === 1.0
      fahrenheit(70.0).to(fahrenheit) must be ~(70.0 +/- err)
    }
    "Have working offsets" in {
      val off = UnitM[Temperature]("","",2.0,100.0)
      val base = UnitM[Temperature]("","")
      off(0.0) ==== base(200.0)
      off(100.0) ==== base(400.0)
      base(400.0).to(off) ==== 100.0
    }
    "Compose" in {
      val mpg2 = mile / gallon
      mpg2(1.0).to(mpg2) === 1.0
      mpg2(1.0).v must be ~(mpg(1.0).v +/- err)
      
      val psi2 = poundForce / squareInch
      psi2(1.0).to(psi2) === 1.0
      psi2(1.0).v must be ~(psi(1.0).v +/- err)
    }
  }
}