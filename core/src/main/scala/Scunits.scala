import scunits.{Scalar, Dims}
import scunits.types._

package object scunits {
  trait Dim
  trait Dims {
    type qlist <: QList
    type values <: qlist#base

    type inv = DimsConst[qlist, qlist#inv[values]]

    class op[R <: DimsOf[qlist]] {
      type mult = DimsConst[qlist, qlist#op[values, R#values, +]]
      type div = DimsConst[qlist, qlist#op[values, R#values, -]]
    }
  }

  object Basis {
    trait Length extends BaseQuantity {
      trait of extends Dim { type length <: Integer }
      type set[To <: Integer] = of { type length = To }
      type get[L <: of] = L#length
    }
    trait Time extends BaseQuantity {
      trait of extends Dim { type time <: Integer }
      type set[To <: Integer] = of { type time = To }
      type get[L <: of] = L#time
    }
    trait Mass extends BaseQuantity {
      trait of extends Dim { type mass <: Integer }
      type set[To <: Integer] = of { type mass = To }
      type get[L <: of] = L#mass
    }
    trait Temperature extends BaseQuantity {
      trait of extends Dim { type temperature <: Integer }
      type set[To <: Integer] = of { type temperature = To }
      type get[L <: of] = L#temperature
    }
    trait AmountOfSubstance extends BaseQuantity {
      trait of extends Dim { type amountOfSubstance <: Integer }
      type set[To <: Integer] = of { type amountOfSubstance = To }
      type get[L <: of] = L#amountOfSubstance
    }
    trait Current extends BaseQuantity {
      trait of extends Dim { type current <: Integer }
      type set[To <: Integer] = of { type current = To }
      type get[L <: of] = L#current
    }
    trait Intensity extends BaseQuantity {
      trait of extends Dim { type intensity <: Integer }
      type set[To <: Integer] = of { type intensity = To }
      type get[L <: of] = L#intensity
    }
    trait Info extends BaseQuantity {
      trait of extends Dim { type info <: Integer }
      type set[To <: Integer] = of { type info = To }
      type get[L <: of] = L#info
    }
  }
  
  class SiBaseQuantities extends (
    Basis.Length :: Basis.Time :: Basis.Mass :: Basis.Temperature ::
    Basis.AmountOfSubstance :: Basis.Current :: Basis.Intensity :: QNil
  )
  implicit val siBaseQuantities = new SiBaseQuantities
  import siBaseQuantities._

  type *[L <: Dims, R <: DimsOf[L#qlist]] = DimsConst[L#qlist, L#values]#op[R]#mult
  type /[L <: Dims, R <: DimsOf[L#qlist]] = DimsConst[L#qlist, L#values]#op[R]#div

  val coef = UnitM[Dimless]("","",1.0)

  type Length = Basis.Length ^ p1
  type Time = Basis.Time ^ p1
  type Mass = Basis.Mass ^ p1
  type Temperature = Basis.Temperature ^ p1
  type AmountOfSubstance = Basis.AmountOfSubstance ^ p1
  type Current = Basis.Current ^ p1
  type Intensity = Basis.Intensity ^ p1
  type Info = Basis.Info ^ p1

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