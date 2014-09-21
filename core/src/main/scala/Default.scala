package scunits

import scunits.types._

trait Quantity
trait BaseQuantity extends Quantity

class TestParam[A] {
  type Apply[X <: A] = X
}
class TestAb {
  type A
  type Apply[X <: A] = X
}
class TestParamNested[A] {
  class Inner {
    type Apply[X <: A] = X
  }
}
class TestAbNested {
  type A
  class Inner {
    type Apply[X <: A] = X
  }
}
object Test{
  // type arguments [Int] do not conform to type Apply's type parameter bounds [X <: A]
  // type T1 = TestParam[Int]#Apply[Int]

  // type arguments [Int] do not conform to type Apply's type parameter bounds [X <: TestAb.this.A]
  // type T2 = (TestAb{type A = Int})#Apply[Int]

  // Compiles
  val T3 = new TestParam[Int]
  type T3 = T3.Apply[Int]  

  // type arguments [Int] do not conform to type Apply's type parameter bounds [X <: A]
  // object T4 extends TestNested[Int]
  // type T4 = T4.Inner#Apply[Int]

  object T5 extends TestAbNested {
    trait A extends Any
    object Inner extends Inner
  }
  // type T5p = T5.Inner#Apply[T5.A]
  type T5d = T5.Inner.Apply[T5.A]

  type TestType[A] = {
    type Apply[X <: A] = X
  }
  type T6 = TestType[Int]#Apply[Int]
}

class DimsConverter[QI <: QList,I <: DList] {
  type QuantsIn = QI
  type Indexes = I
  type QuantsOut <: QList
  type Apply[EI <: DList] <: DList
}

trait Quantities {
  type Quants <: QList

  type Dims                  = types.DimsOf[Quants]
  type DimsOf[D <: DList]    = Quants ^ D
  type DimOf[I <: NonNegInt] = DimsOf[DNil#Set[I,i1]]
  type Dimless = DimsOf[DNil]

  val coef = UnitM[Dimless](mult = 1.0)
  implicit def toCoef(v: Double) = Measure[Dimless](v)
}

package object default extends Quantities {
  object Time extends BaseQuantity
  object Info extends BaseQuantity
  object Length extends BaseQuantity
  object Mass extends BaseQuantity
  object Temperature extends BaseQuantity
  object AmountOfSubstance extends BaseQuantity  
  object Current extends BaseQuantity
  object Intensity extends BaseQuantity

  trait Quants extends
    (Time.type :: Info.type :: Length.type :: Mass.type :: Temperature.type ::
    AmountOfSubstance.type :: Current.type :: Intensity.type :: QNil)  
  
  type Length            = DimOf[i0]
  type Time              = DimOf[i1]
  type Mass              = DimOf[i2]
  type Temperature       = DimOf[i3]
  type AmountOfSubstance = DimOf[i4]
  type Current           = DimOf[i5]
  type Intensity         = DimOf[i6]
  type Info              = DimOf[i7]

  type Area = Length * Length
  type Volume = Area * Length
  type Density = Mass / Volume
  type Speed = Length / Time
  type Acceleration = Speed / Time
  type Frequency = Dimless / Time
  type Force = Mass * Acceleration
  type Pressure = Force / Area
  type Energy = Force * Length
  type Power = Energy / Time
  type VolumeFlow = Volume / Time
  type AngularVelocity = Dimless / Time
  type AngularMomentum = Energy * Time
  type Torque = Force * Length
  type InfoRate = Info / Time

  // Electric:
  type Charge = Current * Time
  type Potential = Power / Current
  type Capacitance = Charge / Potential
  type Resistance = Potential / Current
  type Conductance = Current / Potential
  type Inductance = Flux / Current

  // Magnetic:
  type Flux = Potential * Time
  type FieldStrength = Flux / Area

  // Radioactive:
  type Decay = Frequency
  type Dose = Energy / Mass

  type Illuminance = Intensity / Area

  type CatalyticActivity = AmountOfSubstance / Time

  // Automotive:
  type DistancePerFuelUsed = Length / Volume
}