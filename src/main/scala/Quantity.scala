package scunits

import scunits.integer._

class BaseQuantity[I <: NonNegInt](val name: String, val symbol: String) {
  type Id = I
  type Base = DNel[Dim, DNil]
  type Dim = Dimension[Id, _1]
}

package object quantity {
  object Length extends BaseQuantity[_0]("length", "L")
  object Time extends BaseQuantity[_1]("time", "T")
  object Mass extends BaseQuantity[_2]("mass", "M")
  object Temperature extends BaseQuantity[_3]("temperature", "Θ")
  object AmountOfSubstance extends BaseQuantity[_4]("mole", "N")    
  object Angle extends BaseQuantity[_5]("angle", "")
  object SolidAngle extends BaseQuantity[_6]("solid angle", "")
  object Info extends BaseQuantity[_7]("bit", "b")

  type Length = Length.Base
  type Time = Time.Base
  type Mass = Mass.Base
  type Temperature = Temperature.Base
  type AmountOfSubstance = AmountOfSubstance.Base
  type Angle = Angle.Base
  type SolidAngle = SolidAngle.Base
  type Info = Info.Base

  type *[L <: Dimensions, R <: Dimensions] = L#Mult[R]
  type /[L <: Dimensions, R <: Dimensions] = L#Div[R]

  type Area = Length * Length
  type Volume = Area * Length
  type Density = Volume / Mass
  type Speed = Length / Time
  type Acceleration = Speed / Time
  type Frequency = DNil / Time
  type Force = Mass * Acceleration
  type Pressure = Force / Area
  type Energy = Force * Mass
  type Power = Energy / Time

  object Electric {
    object Current extends BaseQuantity[_8]("electric current", "I")
    type Current = Current.Base
    type Charge = Current * Time
    type Potential = Power / Current
    type Capacitance = Charge / Potential
    type Resistance = Potential / Current
    type Conductance = Current / Potential    
    type Inductance = Magnetic.FieldStrength / Current
  }

  object Magnetic {
    type Flux = Electric.Potential / Time
    type FieldStrength = Flux / Area
    type Inductance = Electric.Inductance
  }

  object Luminous {
    object Intensity extends BaseQuantity[_9]("luminous intensity", "J")
    type Intensity = Intensity.Base
  }
  type Illuminance = Luminous.Intensity / Area

  object Radioactive {
    type Decay = Frequency
    type Dose = Area / (Decay * Decay)
  }

  type CatalyticActivity = AmountOfSubstance / Time
}