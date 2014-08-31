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
  import scunits.unit.time._

  // Get the SI prefixes too:
  import scunits.unit.si.Prefix._

  // import implicit conversions:
  import Scunits._

  // Alas, floating point arithmetic is not exact.
  val err = 0.00000001

  "Measures" should {
    "Be stored as SI units" in {
      // All measures are case value classes, and are stored as SI units, so comparisons between measures produce expected results.
      // Measure is the value class which contains the underlying value (Measure.v). Volume is the dimension (Dims).
      val gal: Measure[Volume] = gallon(1.0)
      val oneLitre: Measure[Volume] = litre(1.0)      

      // gallon, litre and cubicMetre are all units of measure (UnitMs). They convert inputed Doubles to a base SI value.
      // In the case of volume this is cubic metres.
      gal !=== oneLitre
      oneLitre ==== cubicMetre(0.001)

      // Values of the same Dims can be added:
      (gal + oneLitre) must be_> (gal - oneLitre)

      // Use Measure.v to access the underlying double,
      gal.v must beCloseTo(litre(3.78541).v, err)
      // This value represents the Measure in its SI unit, e.g. one gallon is so many cubic metres:
      gal.v ==== 0.00378541

      // Naturally if we do Measure[A] / Measure[A] we get a dimensionless (DNil) result:
      val dimless: Measure[DNil] = gal / oneLitre

      // Type-level Dims composition is easy:
      implicitly[Volume#Div[Length] =:= Area]
      implicitly[Acceleration#Mult[Mass] =:= Force]
      
      // Dims types change as you'd expect:
      val litreArea: Measure[Area] = oneLitre / metre(0.1)
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

  "Algebra" should {
    "Work" in {      
      // Even when dealing with abstract Dims, some elementary algebra is possible. e.g.:

      // Implicitly convert Measure[A] * Measure[B / A] to Measure[B]
      def cancelDenominator[A <: Dims, B <: Dims](a: Measure[A], b: Measure[B#Div[A]]): Measure[B] = a * b
      cancelDenominator[Time,Length](second(1.0), metrePerSecond(60.0)) ==== metre(60.0)
      // Without the implicit conversion, the compiler cannot determine that Measure[A#Mult[B#Div[A]]] = Measure[B]

      // Implicitly convert Measure[A] / (Measure[A] / Measure[B]) to Measure[B]
      def cancelNumerator[A <: Dims, B <: Dims](a: Measure[A], b: Measure[A#Div[B]]): Measure[B] = a / b
      cancelNumerator[Length,Time](metre(60.0), metrePerSecond(60.0)) ==== second(1.0)      
    }
  }
}