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

  val err = 0.00000001

  "Measures" should {
    "Be stored as SI units" in {
      // All measures are case value classes, and are stored as SI units, so comparisons between measures produce expected results.
      val gal: Measure[Volume] = gallon(1.0)
      val l: Measure[Volume] = litre(1.0)
      // Measure is the value class which contains the underlying value (Measure.v). Volume is the dimension (Dims).
      gal !=== l
      l ==== cubicMetre(0.001)

      // Use Measure.v to access the underlying double,
      gal.v must beCloseTo(litre(3.78541).v, err)
      // This value represents the Measure in its SI unit, e.g. one gallon is so many cubic metres:
      gal.v ==== 0.00378541

      // Naturally if we do Measure[A] / Measure[A] we get a dimensionless (DNil) result:
      val dimless: Measure[DNil] = gal / l

      // Type-level Dims composition is easy:
      implicitly[Volume#Div[Length] =:= Area]
      implicitly[Acceleration#Mult[Mass] =:= Force]
      
      // Dims types change as you'd expect:
      val litreArea: Measure[Area] = l / metre(0.1)
      litreArea ==== squareMetre(0.01)
      // This does not compile:
      // litreArea ==== cubicMetre(0.01)
    }
  }

  "Prefixes" should {
    "Work" in {
      // All SI prefixes are included:
      centi(metre, 10) ==== metre(0.1)

      // You can use prefixes to create new units of measure (UnitM), e.g.:
      val decimetre = deci(metre)
      decimetre(10) ==== metre(1)      
    }
  }

  "Units" should {
    "Compose" in {
      // Any UnitM can be composed by multiplication or division with other UnitMs:
      val decimetre = deci(metre)
      val myLitre = decimetre * decimetre * decimetre
      // A litre is a cubic decimetre:
      myLitre(1).v must beCloseTo(litre(1).v, err)

      // Suppose we need UnitMs for fuel consumption:
      val kmpL = kilo(metre) / litre
      val mpg = mile / gallon
      mpg(20.0) ==== kmpL(8.502857022092719)
    }
  }
}