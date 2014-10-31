package scunits.test

import scunits._
import scunits.default._
import scunits.si._

import org.specs2.mutable._

class Scalars extends Specification {
  val m1 = metre(1.0)
  val m2 = metre(2.0)
  "Scalars" should {    
    "Add" in {
      m1 + m2 ==== metre(3.0)
    }
    "Subtract" in {
      m2 - m1 ==== m1
    }
    "Multiply" in {
      m1 * m2 ==== squareMetre(2.0)
    }
    "Divide" in {
      m1 / m2 ==== coef(0.5)
    }
    "Automatically invert" in {
      m2.inv ==== m2
    }
  }
}