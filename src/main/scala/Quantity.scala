package scunits

import scunits.integer._

package object quantity {
  object Length extends BaseQuantity[_0]("length", "L")
  object Time extends BaseQuantity[_1]("time", "T")
  object Mass extends BaseQuantity[_2]("mass", "M")
  object Temperature extends BaseQuantity[_3]("temperature", "Θ")
  object AmountOfSubstance extends BaseQuantity[_4]("mole", "N")    
  object Angle extends BaseQuantity[_5]("angle", "")
  object SolidAngle extends BaseQuantity[_6]("solid angle", "")
  object Bit extends BaseQuantity[_7]("bit", "b")

  type *[L <: Dimensions, R <: Dimensions] = L#Mult[R]
  type /[L <: Dimensions, R <: Dimensions] = L#Div[R]

  type Area = Length.Base * Length.Base
  type Volume = Area * Length.Base
  type Density = Volume / Mass.Base
  type Speed = Length.Base / Time.Base
  type Acceleration = Speed / Time.Base
  type Frequency = DNil / Time.Base
  type Force = Mass.Base * Acceleration
  type Pressure = Force / Area
  type Energy = Force * Mass.Base
  type Power = Energy / Time.Base

  object Electric {
    object Current extends BaseQuantity[_8]("electric current", "I")
    type Charge = Current.Base * Time.Base
    type Potential = Power / Current.Base
    type Capacitance = Charge / Potential
    type Resistance = Potential / Current.Base
    type Conductance = Current.Base / Potential    
    type Inductance = Magnetic.FieldStrength / Current.Base
  }

  object Magnetic {
    type Flux = Electric.Potential / Time.Base
    type FieldStrength = Flux / Area
    type Inductance = Electric.Inductance
  }

  object Luminous {
    object Intensity extends BaseQuantity[_9]("luminous intensity", "J")
  }
  type Illuminance = Luminous.Intensity.Base / Area

  object Radioactive {
    type Decay = Frequency
    type Dose = Area / (Decay * Decay)
  }

  type CatalyticActivity = AmountOfSubstance.Base / Time.Base
}