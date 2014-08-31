package scunits.test

import org.specs2.mutable._

class Examples extends Specification {
  // Import Measures, UnitMs, BaseQuantity, etc.
  import scunits._

  // Import the pre-defined (SI) base quantities:
  import scunits.quantity._

  // Import American, SI and metric units:
  import scunits.unit.us._
  import scunits.unit.si._
  import scunits.unit.metric._

  // Get the SI prefixes too:
  import scunits.unit.si.Prefix._

  "Measures" should {
    "Be stored as SI units" in {
      // All measures are case value classes, and are stored as SI units, so comparisons between measures produce expected results.
      val gal: Measure[Volume] = gallon(1.0)
      val l: Measure[Volume] = litre(1.0)
      gal !=== l
      l ==== cubicMetre(0.001)

      // Use Measure.v to access the underlying double:
      gal.v must beCloseTo(litre(3.78541).v, 0.00000001)
      
      // Measure types change as you'd expect:
      val litreArea: Measure[Area] = l / metre(0.1)
      litreArea ==== squareMetre(0.01)
    }
  }

  "Prefixes" should {
    "Work" in {
      // All SI prefixes are included:
      centi(metre, 10) ==== metre(0.1)

      // You can use prefixes to create new units, e.g.:
      val centimetre = centi(metre)
      centimetre(10) ==== metre(0.1)

      // Then compose those units:
      // val myLitre = centimetre.mult("centimetre","cm")(centimetre,centimetre)
    }
  }

  "Units" should {
    "Compose" in {
      // Suppose we need units for fuel consumption:
      val kmpL = kilo(metre) / litre
      val mpg = mile / gallon
      mpg(20.0) ==== kmpL(8.502857022092719)
    }
  }
}