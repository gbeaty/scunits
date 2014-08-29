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

  type Area = DNel[Length.type,_2,DNil] // Length#Mult[Length]
  type Volume = DNel[Length.type,_3,DNil] // Area#Mult[Length]
  type Density = DNel[Length.type,_3#Neg,DNel[Mass.type,_1,DNil]] // Mass#Div[Volume]
  type Speed = DNel[Length.type,_1,DNel[Time.type,_1#Neg,DNil]] // Length#Div[Time]
  type Acceleration = DNel[Length.type,_1,DNel[Time.type,_2#Neg,DNil]] // Speed#Div[Time]
  type Frequency = DNel[Time.type,_1#Neg,DNil] // DNil#Div[Time]
  type Force = DNel[Length.type,_1,DNel[Time.type,_2#Neg,DNel[Mass.type,_1,DNil]]] // Mass#Mult[Acceleration]
  type Pressure = DNel[Length.type,_1#Neg,DNel[Time.type,_2#Neg,DNel[Mass.type,_1,DNil]]] // Force#Div[Area]
  type Energy = DNel[Length.type,_2,DNel[Time.type,_2#Neg,DNel[Mass.type,_1,DNil]]] // Force#Mult[Mass]
  type Power = DNel[Length.type,_2,DNel[Time.type,_3#Neg,DNel[Mass.type,_1,DNil]]] // Energy#Div[Time]

  object Electric {
    object Current extends BaseQuantity[_8]("electric current", "I")
    type Current = Current.Base
    type Charge = DNel[Time.type,_1,DNel[Current.type,_1, DNil]] // Current#Mult[Time]
    type Potential = DNel[Length.type,_2,DNel[Time.type,_3#Neg,DNel[Mass.type,_1,DNel[Current.type,_1#Neg,DNil]]]] // Power#Div[Current]
    type Capacitance = DNel[Length.type,_2#Neg,DNel[Time.type,_4,DNel[Mass.type,_1#Neg,DNel[Current.type,_2,DNil]]]]
    // kg−1⋅m−2⋅s4⋅A2
    type Resistance = DNel[Length.type,_2,DNel[Time.type,_3#Neg,DNel[Mass.type,_1,DNel[Current.type,_2#Neg,DNil]]]] // Potential#Div[Current]
    type Conductance = DNel[Length.type,_2#Neg,DNel[Time.type,_3,DNel[Mass.type,_1#Neg,DNel[Current.type,_2,DNil]]]] // Current#Div[Potential]
    type Inductance = DNel[Length.type,_2,DNel[Time.type,_2#Neg,DNel[Mass.type,_1,DNel[Current.type,_2#Neg,DNil]]]] // Magnetic.FieldStrength#Div[Current]
  }

  object Magnetic {
    import Electric.Current
    type Flux = DNel[Length.type,_2,DNel[Time.type,_2#Neg,DNel[Mass.type,_1,DNel[Current.type,_1#Neg,DNil]]]] // Electric.Potential#Div[Time]
    type FieldStrength = DNel[Time.type,_2#Neg,DNel[Mass.type,_1,DNel[Current.type,_1#Neg,DNil]]] // Flux#Div[Area]
    type Inductance = Electric.Inductance
  }

  object Luminous {
    object Intensity extends BaseQuantity[_9]("luminous intensity", "J")
    type Intensity = Intensity.Base
  }
  type Illuminance = Luminous.Intensity#Div[Area]

  object Radioactive {
    type Decay = Frequency
    type Dose = DNel[Length.type,_2,DNel[Time.type,_2#Neg,DNil]]
  }

  type CatalyticActivity = AmountOfSubstance#Div[Time]
}