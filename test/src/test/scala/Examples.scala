package scunits.test

import org.specs2.mutable._

class Examples extends Specification {
  // Main import, grabs everything you usually need except specific units:
  import scunits._

  // Import all base SI units, accepted units and prefixes:
  import scunits.si._

  // Import American units:
  import scunits.us._

  // Default to fluid volumes:
  import scunits.us.Fluid._

  "Scalars" should {
    "Work" in {
      // All measures are case value classes, and are stored as SI units,
      // so comparisons between measures produce expected results.
      // Scalar is the value class which contains the underlying value (Scalar.v).
      // Volume is the dimension, which is represented by the Dims type.
      val gal: Scalar[Volume] = gallon(1.0)
      val oneLitre: Scalar[Volume] = litre(1.0)

      // gallon, litre and cubicMetre are all units of measure (UnitMs). They convert inputed Doubles to a base SI value.
      // In the case of volume this is cubic metres.
      gal !=== oneLitre
      oneLitre ==== cubicMetre(0.001)

      // Values of the same Dims can be added and subtracted:
      (gal + oneLitre) must be_> (gal - oneLitre)

      // Use Scalar.v to access the underlying double,
      gal ==== litre(3.785411784)
      // This value represents the Scalar in its SI unit, e.g. one gallon is so many cubic metres:
      gal.v ==== 0.003785411784

      // Naturally if we do Scalar[A] / Scalar[A] we get a dimensionless (Dimless) result:
      val dimless: Scalar[Dimless] = gal / oneLitre
      
      // Dims types change as you'd expect:
      val litreArea: Scalar[Area] = oneLitre / metre(0.1)
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
      // ...but don't use it. Doing this creates an entirely new centimetre unit then creats a Scalar of 10 centimetres.
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
      val gpm = mpg.inv
      mpg(20.0) ==== gpm(1.0 / 20.0)
    }
  }

  "Dimensions" should {
    "Be computable" in {
      // Dimensions are represented by the type Dims, and are stored as sets of base quantities.
      // They exist only at the type-level, and have no run-time representation.
      // A dimensionless quantity is represented by the type Dimless:
      val dnil: Scalar[Dimless] = 5.0

      // Type-level Dims computation is easy. We just need to import our QList's operators:
      import scunits.siBaseQuantities.ops._
      implicitly[Volume / Length =:= Area]
      implicitly[Acceleration * Mass =:= Force]

      // Use #neg to find the reciprocal of a Dims:
      val hz: Scalar[siBaseQuantities.neg[Time]] = hertz(5.0)

      // Dims compose as you might expect:
      Scalar[Length *Length](4.0) ==== metre(2.0) * metre(2.0)
    }
  }

  "Base Quantities" should {
    "Be definible" in {
      true
    }
  }
}