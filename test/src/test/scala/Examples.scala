package scunits.test

import org.specs2.mutable._

class Examples extends Specification {
  // Import Measures, UnitMs, BaseQuantity, etc.
  import scunits._

  // Import all base SI units, accepted units and prefixes:
  import scunits.si._

  // Import American units:
  import scunits.us._

  // Default to fluid volumes:
  import scunits.us.Fluid._

  "Measures" should {
    "Work" in {
      // All measures are case value classes, and are stored as SI units,
      // so comparisons between measures produce expected results.
      // Measure is the value class which contains the underlying value (Measure.v).
      // Volume is the dimension, which is represented by the Dims type.
      val gal: Measure[Volume] = gallon(1.0)
      val oneLitre: Measure[Volume] = litre(1.0)

      // gallon, litre and cubicMetre are all units of measure (UnitMs). They convert inputed Doubles to a base SI value.
      // In the case of volume this is cubic metres.
      gal !=== oneLitre
      oneLitre ==== cubicMetre(0.001)

      // Values of the same Dims can be added and subtracted:
      // (gal + oneLitre) must be_> (gal - oneLitre)

      // Use Measure.v to access the underlying double,
      gal ==== litre(3.785411784)
      // This value represents the Measure in its SI unit, e.g. one gallon is so many cubic metres:
      gal.v ==== 0.003785411784

      // Naturally if we do Measure[A] / Measure[A] we get a dimensionless (Dimless) result:
      val dimless: Measure[Dimless] = gal / oneLitre

      // Type-level Dims composition is easy:
      // implicitly[Volume#Div[Length] =:= Area]
      // implicitly[Acceleration#Mult[Mass] =:= Force]
      
      // Dims types change as you'd expect:
      val litreArea: Measure[Area] = oneLitre / metre(0.1)
      litreArea ==== squareMetre(0.01)

      // This does not compile:
      // litreArea ==== cubicMetre(0.01)

      // Automagically recognize and convert inverse units:
      val massPerVolumeOfWater = pound(8.33) / gallon(1.0)
      val volumePerMassOfWater = gallon(1.0) / pound(8.33)
      // massPerVolumeOfWater   ==== volumePerMassOfWater
      // Their values will be different because Frequency is dimensionally different from Time:
      massPerVolumeOfWater.v !=== volumePerMassOfWater.v
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
      // ...but don't use it. Doing this creates an entirely new centimetre unit then creats a Measure of 10 centimetres.
      // This is a much more costly operation than the centi(metre, 10) example,
      // which only results in some Double multiplication.

      // To create a new prefix:
      val myCenti = UnitPrefix("my-centi","mc",0.01)
      myCenti(metre, 10) ==== centi(metre, 10)
    }
  }

  "Units" should {
    "Compose" in {
      // Units of measure are represented by UnitMs in scunits.
      // Any UnitM can be composed by multiplication or division with other UnitMs:
      val decimetre = deci(metre)
      val myLitre = decimetre * decimetre * decimetre
      // A litre is a cubic decimetre:
      myLitre(1) ==== litre(1)

      // Suppose we need UnitMs for fuel consumption:
      val kmpL = kilo(metre) / litre
      val mpg = mile / gallon                          
      mpg(20.0) ==== kmpL(8.50287414860544)

      // But distance traveled per fuel used is a poor way to represent gas milage. Fuel used per distance is better.
      // Invert a UnitM with .inv:
      // val gpm: UnitM[Volume#Div[Length]] = mpg.inv
      // mpg(20.0) ==== gpm(1.0 / 20.0)
    }
  }

  "Dimensions" should {
    "Be composable" in {
      // Dimensions are represented by the type Dims, and are stored as lists of base quantities.
      // They exist only at the type-level, and have no run-time representation.

      // Dims can be DNels (non-empty list), which are non-nil dimensions:
      // def sq[D <: DNel](in: Measure[D]) = in * in
      // So this will compile:
      // sq(metre(2.0)) ==== squareMetre(4.0)
      // ...but this won't:
      // sq(coef(2.0)) ==== coef(4.0)

      // ...or Dimless, which are empty lists of Dims and represent dimensionless quantites:
      val Dimless: Measure[Dimless] = 5.0

      // Use #Neg to find the reciprocal of a Dims:
      val hz: Measure[Time#Neg] = hertz(5.0)

      // Dims compose as you might expect:
      val sqm: Measure[Area] = metre(2.0) * metre(2.0)
      // sqm ==== squareMetre(4.0)
      // val m: Measure[Volume] = sqm / metre(4.0)
      // m ==== metre(1.0)
      true
    }
  }

  "Base Quantities" should {
    "Be definible" in {
      true
    }
  }

  /*"Algebra" should {
    "Work on abstract Measures" in {      
      // Even when dealing with abstract Dims, some elementary algebra is possible. e.g.:

      // Implicitly convert Measure[A] * Measure[B / A] to Measure[B]
      def cancelDenominator[L <: Dims, R <: Dims](l: Measure[L], r: Measure[R#Div[L]]): Measure[R] = l * r
      cancelDenominator[Time,Length](second(1.0), metrePerSecond(60.0)) ==== metre(60.0)

      // Implicitly convert Measure[A] / (Measure[A] / Measure[B]) to Measure[B]
      def cancelNumerator[A <: Dims, B <: Dims](a: Measure[A], b: Measure[A#Div[B]]): Measure[B] = a / b
      cancelNumerator[Length,Time](metre(60.0), metrePerSecond(60.0)) ==== second(1.0)

      // A / A = a dimensionless quantity
      def cancelSelf[A <: Dims](a: Measure[A]): Measure[Dimless] = a / a
      cancelSelf[Length](metre(1.0)) ==== Measure[Dimless](1.0)

      // More complex algebra does not work, yet:
      // def abOverAc[A <: Dims, B <: Dims, C <: Dims](l: Measure[A#Mult[B]], r: Measure[A#Mult[C]]): Measure[B#Div[A]] = l / r
    }
  }*/
}