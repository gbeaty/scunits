package scunits.test

import org.specs2.mutable._

class Examples extends Specification {
  // Import Measures, UnitMs, BaseQuantity, etc.
  import scunits._
  import Scunits._

  // Import the pre-defined base quantities:
  import scunits.quantity._

  // Import all base SI units, accepted units and prefixes:
  import scunits.si._

  // Import American units:
  import scunits.us._

  // Default to fluid volumes:
  import scunits.us.Fluid._

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

      // Values of the same Dims can be added and subtracted:
      (gal + oneLitre) must be_> (gal - oneLitre)

      // Use Measure.v to access the underlying double,
      gal.v must beCloseTo(litre(3.78541).v, err)
      // This value represents the Measure in its SI unit, e.g. one gallon is so many cubic metres:
      gal.v ==== 0.003785411784

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

      // This syntax also works:
      centi(metre)(10) ==== metre(0.1)
      // ...but don't use it. Doing this creates an entirely new centimetre unit then creating a Measure of 10 centimetres.
      // This is a much more costly operation than the centi(metre, 10) example, which really amounts to some Double multiplication.

      // To create a new prefix:
      val myCenti = UnitPrefix("my-centi","mc",0.01)
      myCenti(metre, 10) ==== centi(metre, 10)
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
      mpg(20.0) ==== kmpL(8.502874157590327)
    }
  }

  "Algebra" should {
    "Work on abstract Measures" in {      
      // Even when dealing with abstract Dims, some elementary algebra is possible. e.g.:

      // import implicit conversions:
      import Scunits._

      // Implicitly convert Measure[A] * Measure[B / A] to Measure[B]
      def cancelDenominator[L <: Dims, R <: Dims](l: Measure[L], r: Measure[R#Div[L]]): Measure[R] = l * r
      cancelDenominator[Time,Length](second(1.0), metrePerSecond(60.0)) ==== metre(60.0)

      // Implicitly convert Measure[A] / (Measure[A] / Measure[B]) to Measure[B]
      def cancelNumerator[A <: Dims, B <: Dims](a: Measure[A], b: Measure[A#Div[B]]): Measure[B] = a / b
      cancelNumerator[Length,Time](metre(60.0), metrePerSecond(60.0)) ==== second(1.0)

      // A / A = a dimensionless quantity
      def cancelSelf[A <: Dims](a: Measure[A]): Measure[DNil] = a / a
      cancelSelf[Length](metre(1.0)) ==== Measure[DNil](1.0)
    }
  }
}