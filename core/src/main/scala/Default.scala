package scunits

import scunits.types._

trait DefaultQuantities extends Quantities {
  trait Quants extends
    (Time.type :: Info.type :: Length.type :: Mass.type :: Temperature.type ::
    Current.type :: Intensity.type :: AmountOfSubstance.type :: QNil)  
  
  type Time              = dimOf[i0]
  type Info              = dimOf[i1]
  type Length            = dimOf[i2]
  type Mass              = dimOf[i3]
  type Temperature       = dimOf[i4]
  type Current           = dimOf[i5]
  type Intensity         = dimOf[i6]
  type AmountOfSubstance = dimOf[i7]

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

package object default extends DefaultQuantities