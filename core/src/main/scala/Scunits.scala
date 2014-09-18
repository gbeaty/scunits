import scunits._
import scunits.types._

trait LowPriorityImplicits {
  object Mult
  object Div
  implicit def multDims[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R], op: Mult.type) = Measure[L#Mult[R]](l.v * r.v)
  implicit def divDims[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R], op: Div.type) = Measure[L#Div[R]](l.v / r.v)
}

package object scunits extends LowPriorityImplicits {
  implicit def ordering[D <: Dims] = new Ordering[Measure[D]] {
    def compare(l: Measure[D], r: Measure[D]) = if(l < r) -1 else if(l > r) 1 else 0
  }
  
  implicit def mult_L_DNil[L <: Dims](l: Measure[L], r: Measure[DNil], op: Mult.type) = Measure[L](l.v * r.v)
  implicit def mult_DNil_L[L <: Dims](l: Measure[DNil], r: Measure[L], op: Mult.type) = Measure[L](l.v * r.v)
  implicit def mult_L_RdivL[L <: Dims,R <: Dims](l: Measure[L], r: Measure[R#Div[L]], op: Mult.type) = Measure[R](l.v * r.v)
  implicit def mult_LdivR_R[L <: Dims,R <: Dims](l: Measure[L#Div[R]], r: Measure[R], op: Mult.type) = Measure[L](l.v * r.v)

  implicit def div_L_DNil[L <: Dims](l: Measure[L], r: Measure[DNil], op: Div.type) = Measure[L](l.v / r.v)
  implicit def div_DNil_R[R <: Dims](l: Measure[DNil], r: Measure[R], op: Div.type) = Measure[R#Neg](l.v / r.v)
  implicit def div_L_LdivR[L <: Dims,R <: Dims](l: Measure[L], r: Measure[L#Div[R]], op: Div.type) = Measure[R](l.v / r.v)
  implicit def div_RdivL_R[L <: Dims,R <: Dims](l: Measure[R#Div[L]], r: Measure[R], op: Div.type) = Measure[L#Neg](l.v / r.v)
  implicit def div_L_L[L <: Dims](l: Measure[L], r: Measure[L], op: Div.type) = Measure[DNil](l.v / r.v)  

  implicit def invert[F <: Dims](f: Measure[F]) = Measure[F#Neg](1.0 / f.v)
  
  val coef = UnitM[DNil]("","")

  implicit def toCoef(d: Double) = Measure[DNil](d)

  object Length extends BaseQuantity[_0]("length", "L")
  object Time extends BaseQuantity[_1]("time", "T")
  object Mass extends BaseQuantity[_2]("mass", "M")
  object Temperature extends BaseQuantity[_3]("temperature", "Θ")
  object AmountOfSubstance extends BaseQuantity[_4]("mole", "N")    
  // object Angle extends BaseQuantity[_5]("angle", "")
  // object SolidAngle extends BaseQuantity[_6]("solid angle", "")
  object Info extends BaseQuantity[_7]("info", "")

  type Length = Length.Base
  type Time = Time.Base
  type Mass = Mass.Base
  type Temperature = Temperature.Base
  type AmountOfSubstance = AmountOfSubstance.Base
  type Angle = DNil // type Angle = Angle.Base
  type SolidAngle = DNil // type SolidAngle = SolidAngle.Base
  type Info = Info.Base

  type Area = Length#Mult[Length]
  type Volume = Area#Mult[Length]
  type Density = Mass#Div[Volume]
  type Speed = Length#Div[Time]
  type Acceleration = Speed#Div[Time]
  type Frequency = DNil#Div[Time]
  type Force = Mass#Mult[Acceleration]
  type Pressure = Force#Div[Area]
  type Energy = Force#Mult[Length]
  type Power = Energy#Div[Time]
  type VolumeFlow = Volume#Div[Time]
  type AngularVelocity = DNil#Div[Time]
  type AngularMomentum = Energy#Mult[Time]
  type Torque = Force#Mult[Length]

  type InfoRate = Info#Div[Time]

  object Electric {
    object Current extends BaseQuantity[_8]("electric current", "I")
    type Current = Current.Base
    type Charge = Current#Mult[Time]
    type Potential = Power#Div[Current]
    type Capacitance = Charge#Div[Potential]
    type Resistance = Potential#Div[Current]
    type Conductance = Current#Div[Potential]
    type Inductance = Magnetic.Flux#Div[Current]
  }

  object Magnetic {
    import Electric.Current
    type Flux = Electric.Potential#Mult[Time]
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
    type Dose = Energy#Div[Mass]
  }

  object Automotive {
    type DistancePerFuel = Length#Div[Volume]
  }

  type CatalyticActivity = AmountOfSubstance#Div[Time]
}