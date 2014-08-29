package scunits

import scunits.integer._

sealed trait BaseQuantityLike {
  type Id <: NonNegInt
  type Base = DNel[this.type, _1, DNil]
}
class BaseQuantity[I <: NonNegInt](val name: String, val symbol: String) extends BaseQuantityLike {
  type Id = I
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

  type Area = Length#Mult[Length]
  type Volume = Area#Mult[Length]
  type Density = Volume#Div[Mass]
  type Speed = Length#Div[Time]
  type Acceleration = Speed#Div[Time]
  type Frequency = DNil#Div[Time]
  type Force = Mass#Mult[Acceleration]
  type Pressure = Force#Div[Area]
  type Energy = Force#Mult[Mass]
  type Power = Energy#Div[Time]

  object Electric {
    object Current extends BaseQuantity[_8]("electric current", "I")
    type Current = Current.Base
    type Charge = Current#Mult[Time]
    type Potential = Power#Div[Current]
    type Capacitance = Charge#Div[Potential]
    type Resistance = Potential#Div[Current]
    type Conductance = Current#Div[Potential]
    type Inductance = Magnetic.FieldStrength#Div[Current]
  }

  object Magnetic {
    type Flux = Electric.Potential#Div[Time]
    type FieldStrength = Flux#Div[Area]
    type Inductance = Electric.Inductance
  }

  object Luminous {
    object Intensity extends BaseQuantity[_9]("luminous intensity", "J")
    type Intensity = Intensity.Base
  }
  type Illuminance = Luminous.Intensity#Div[Area]

  object Radioactive {
    type Decay = Frequency
    type Dose = Area#Div[(Decay#Mult[Decay])]
  }

  type CatalyticActivity = AmountOfSubstance#Div[Time]
}