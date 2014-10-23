import scunits.{Scalar, Dims, Dimless}
import scunits.types._

trait Implicits {
  implicit def invert[D <: Dims, Qs <: QList](s: Scalar[D])(implicit qs: Qs) = s.inv
  implicit def toCoef(d: Double) = Scalar[Dimless](d)
}

package object scunits extends Implicits {
  trait Dims
  type Dimless = Dims

  object Basis {
    trait Length extends BaseQuantity {
      trait of { type length <: Integer }
      type set[To <: Integer] = of { type length = To }
      type get[L <: of] = L#length
    }
    trait Time extends BaseQuantity {
      trait of { type time <: Integer }
      type set[To <: Integer] = of { type time = To }
      type get[L <: of] = L#time
    }
    trait Mass extends BaseQuantity {
      trait of { type mass <: Integer }
      type set[To <: Integer] = of { type mass = To }
      type get[L <: of] = L#mass
    }
    trait Temperature extends BaseQuantity {
      trait of { type temperature <: Integer }
      type set[To <: Integer] = of { type temperature = To }
      type get[L <: of] = L#temperature
    }
    trait AmountOfSubstance extends BaseQuantity {
      trait of { type amountOfSubstance <: Integer }
      type set[To <: Integer] = of { type amountOfSubstance = To }
      type get[L <: of] = L#amountOfSubstance
    }
    trait Current extends BaseQuantity {
      trait of { type current <: Integer }
      type set[To <: Integer] = of { type current = To }
      type get[L <: of] = L#current
    }
    trait Intensity extends BaseQuantity {
      trait of { type intensity <: Integer }
      type set[To <: Integer] = of { type intensity = To }
      type get[L <: of] = L#intensity
    }
    trait Info extends BaseQuantity {
      trait of { type info <: Integer }
      type set[To <: Integer] = of { type info = To }
      type get[L <: of] = L#info
    }
  }
  
  type SiBaseQuantities =
    Basis.Length :: Basis.Time :: Basis.Mass :: Basis.Temperature ::
    Basis.AmountOfSubstance :: Basis.Current :: Basis.Intensity :: QNil
  implicit val siBaseQuantities = new SiBaseQuantities
  import siBaseQuantities.ops._

  val coef = UnitM[Dimless]("","",1.0)

  type Length = Basis.Length#dim
  type Time = Basis.Time#dim
  type Mass = Basis.Mass#dim
  type Temperature = Basis.Temperature#dim
  type AmountOfSubstance = Basis.AmountOfSubstance#dim
  type Current = Basis.Current#dim
  type Intensity = Basis.Intensity#dim
  type Info = Basis.Info#dim

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

  type Bandwidth = Info / Time
    
  type Charge = Current * Time
  type Voltage = Power / Current
  type Capacitance = Charge / Voltage
  type Resistance = Voltage / Current
  type Conductance = Current / Voltage
  type Inductance = Flux / Current

  type Flux = Voltage * Time
  type FieldStrength = Flux / Area

  type Illuminance = Intensity / Area

  type Decay = Frequency
  type Dose = Energy / Mass

  type CatalyticActivity = AmountOfSubstance / Time

  type DistancePerFuel = Length / Volume
}