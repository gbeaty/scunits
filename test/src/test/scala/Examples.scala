package scunits.test

import org.specs2.mutable._

class Examples extends Specification {
  import scunits._          // Main import, just the basic types.
  import scunits.default._  // Default dimensions (Length, Speed, etc.)  
  import scunits.si._       // Import all base SI units, accepted units and prefixes.  
  import scunits.us._       // Import American units.  
  import scunits.us.Fluid._ // Default to fluid volumes.

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
      (gal + oneLitre) must be_> (gal - oneLitre)

      // Use Measure.v to access the underlying double,
      gal ==== litre(3.785411784)
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

      // Automagically recognize and convert inverse units:
      val massPerVolumeOfWater = pound(8.33) / gallon(1.0)
      val volumePerMassOfWater = gallon(1.0) / pound(8.33)
      massPerVolumeOfWater   ==== volumePerMassOfWater
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
      val gpm: UnitM[Volume#Div[Length]] = mpg.inv
      mpg(20.0) ==== gpm(1.0 / 20.0)
    }
  }

  "Dimensions" should {
    "Be composable" in {
      // Dimensions are represented by the type Dims, and are stored as lists of base quantities.
      // They exist only at the type-level, and have no run-time representation.

      // Dims can be DNels (non-empty list), which are non-nil dimensions:
      def sq[D <: DNel](in: Measure[D]) = in * in
      // So this will compile:
      sq(metre(2.0)) ==== squareMetre(4.0)
      // ...but this won't:
      // sq(coef(2.0)) ==== coef(4.0)

      // ...or DNil, which are empty lists of Dims and represent dimensionless quantites:
      val dnil: Measure[DNil] = 5.0

      // Use #Neg to find the reciprocal of a Dims:
      val hz: Measure[Time#Neg] = hertz(5.0)

      // Dims compose as you might expect:
      val sqm: Measure[Length#Mult[Length]] = metre(2.0) * metre(2.0)
      sqm ==== squareMetre(4.0)
      val m: Measure[Area#Div[Length]] = sqm / metre(4.0)
      m ==== metre(1.0)      
    }
  }

  /*"Base Quantities" should {
    "Be definible" in {
      // We can make up our own base quantities.
      // Unfortunately they must all be given unique type-level numbers as IDs:
      import scunits.integer._
      type _10 = SuccInt[_9]
      type _11 = SuccInt[_10]
      object Apple extends BaseQuantity[_10]("apples","a")
      object Orange extends BaseQuantity[_11]("oranges","o")

      // Then a type alias for each base dimension:
      type Apple = Apple.Base
      type Orange = Orange.Base

      // You can't compare apple and oranges! This won't compile:
      // Measure[Apple](4) > Measure[Orange](2)

      // Under the hood, UnitM.apply converts a number to a base unit for their dimension.
      // Generally this is SI units, but the SI sadly lacks a base quantity for apples.
      // We'll use one apple as the base unit for apples:
      val apple = UnitM[Apple]("apple","a",1)
      apple(1) ==== Measure[Apple](1)
      // So we don't really need the apple UnitM, but I like to have it in case the base unit of Apple changes.

      // Lets define a bushel as 126 apples:
      val bushel = (apple * 126)

      // All base quantities can be composed with the others, e.g., the average apple weighs 150 grams:
      val meanAppleMass = gram(150) / Measure[Apple](1)
      meanAppleMass ==== Measure[Mass#Div[Apple]](150)

      // So we can get the average weight of a bushel of apples:
      bushel(1) * meanAppleMass ==== gram(18900)

      // I'm looking for a way to make base quantities more composable, so there can't be collisions between their IDs.
      // If anyone has any suggestions on how to do this, please let me know.
    }
  }

  "Algebra" should {
    "Work on abstract Measures" in {      
      // Even when dealing with abstract Dims, some elementary algebra is possible. e.g.:

      // Implicitly convert Measure[A] * Measure[B / A] to Measure[B]
      def cancelDenominator[L <: Dims, R <: Dims](l: Measure[L], r: Measure[R#Div[L]]): Measure[R] = l * r
      cancelDenominator[Time,Length](second(1.0), metrePerSecond(60.0)) ==== metre(60.0)

      // Implicitly convert Measure[A] / (Measure[A] / Measure[B]) to Measure[B]
      def cancelNumerator[A <: Dims, B <: Dims](a: Measure[A], b: Measure[A#Div[B]]): Measure[B] = a / b
      cancelNumerator[Length,Time](metre(60.0), metrePerSecond(60.0)) ==== second(1.0)

      // A / A = a dimensionless quantity
      def cancelSelf[A <: Dims](a: Measure[A]): Measure[DNil] = a / a
      cancelSelf[Length](metre(1.0)) ==== Measure[DNil](1.0)

      // More complex algebra does not work, yet:
      // def abOverAc[A <: Dims, B <: Dims, C <: Dims](l: Measure[A#Mult[B]], r: Measure[A#Mult[C]]): Measure[B#Div[A]] = l / r
    }
  }*/
}