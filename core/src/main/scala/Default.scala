package scunits

import scunits.types._

trait DefaultQuantities extends Quantities {
  object Time extends BaseQuantity
  object Info extends BaseQuantity
  object Length extends BaseQuantity
  object Mass extends BaseQuantity
  object Temperature extends BaseQuantity
  object AmountOfSubstance extends BaseQuantity  
  object Current extends BaseQuantity
  object Intensity extends BaseQuantity

  class quants extends
    (Time.type :: Info.type :: Length.type :: Mass.type :: Temperature.type ::
    Current.type :: Intensity.type :: AmountOfSubstance.type :: QNil)
  
  type Time              = dimOf[_0]
  type Info              = dimOf[p1]
  type Length            = dimOf[p2]
  type Mass              = dimOf[p3]
  type Temperature       = dimOf[p4]
  type Current           = dimOf[p5]
  type Intensity         = dimOf[p6]
  type AmountOfSubstance = dimOf[p7]

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