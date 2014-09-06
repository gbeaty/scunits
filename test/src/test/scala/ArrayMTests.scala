package scunits.test

import scunits._
import scunits.quantity._
import scunits.unit.si.base._
import scunits.unit.us.all._

import org.specs2.mutable._

class ArrayMTests extends Specification {
  sequential
  "Arrays of measurements" should {
    val a = ArrayM[Length](Array(1.0,2.0,3.0))
    "Be created correctly" in {
      a(0) ==== metre(1.0)
      a(1) ==== metre(2.0)
      a(2) ==== metre(3.0)
      a(3) must throwA[IndexOutOfBoundsException]
    }
    "Be modified correctly" in {
      a(0) = metre(0.0)
      a(0) ==== metre(0.0)
      a(1) ==== metre(2.0)
      a(2) ==== metre(3.0)
    }
  }
}