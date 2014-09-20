package scunits

import scunits.types._

trait Quantity
trait BaseQuantity extends Quantity
trait DefaultQuantity extends BaseQuantity

trait DefaultQuantities {
  trait Length extends DefaultQuantity
  trait Time extends DefaultQuantity
  trait Mass extends DefaultQuantity
  trait Temperature extends DefaultQuantity
  trait AmountOfSubstance extends DefaultQuantity
  trait Info extends DefaultQuantity
  trait Current extends DefaultQuantity
  trait Intensity extends DefaultQuantity

  type All = Length :: Time :: Mass :: Temperature :: AmountOfSubstance :: Current :: Intensity :: Info :: QNil
}

trait Dims {
  type Quants <: QList
  type Exps <: DList

  type Mult[R <: Dims] <: DimsOf[Quants]
  type Div[R <: Dims] <: DimsOf[Quants]
  type Neg <: DimsOf[Quants]
}
trait DimsOf[Q <: QList] extends Dims {
  type Quants = Q
}
class ^[L <: QList, R <: DList] extends DimsOf[L] {
  type Exps = R

  type Mult[R <: Dims] = Quants ^ Exps#Mult[R#Exps]
  type Div[R <: Dims] = Quants ^ Exps#Mult[R#Exps#Neg]
  type Neg = Quants ^ Exps#Neg
}

class DimsConverter[QI <: QList,I <: DList] {
  type QuantsIn = QI
  type Indexes = I
  type QuantsOut <: QList
  type Apply[EI <: DList] <: DList
}

package object default {
  type DimsOf[D <: DList]    = DefaultQuantities#All ^ D
  type DimOf[I <: NonNegInt] = DimsOf[DNil#Set[I,i1]]

  type Dimless           = DimsOf[DNil]
  type Length            = DimOf[i0]
  type Time              = DimOf[i1]
  type Mass              = DimOf[i2]
  type Temperature       = DimOf[i3]
  type AmountOfSubstance = DimOf[i4]
  type Current           = DimOf[i5]
  type Intensity         = DimOf[i6]
  type Info              = DimOf[i7]

  type Area = Length#Mult[Length]
  type Volume = Area#Mult[Length]
  type Density = Mass#Div[Volume]
  type Speed = Length#Div[Time]
  type Acceleration = Speed#Div[Time]
  type Frequency = Dimless#Div[Time]
  type Force = Mass#Mult[Acceleration]
  type Pressure = Force#Div[Area]
  type Energy = Force#Mult[Length]
  type Power = Energy#Div[Time]
  type VolumeFlow = Volume#Div[Time]
  type AngularVelocity = Dimless#Div[Time]
  type AngularMomentum = Energy#Mult[Time]
  type Torque = Force#Mult[Length]
  type InfoRate = Info#Div[Time]

  // Electric:
  type Charge = Current#Mult[Time]
  type Potential = Power#Div[Current]
  type Capacitance = Charge#Div[Potential]
  type Resistance = Potential#Div[Current]
  type Conductance = Current#Div[Potential]
  type Inductance = Flux#Div[Current]

  // Magnetic:
  type Flux = Potential#Mult[Time]
  type FieldStrength = Flux#Div[Area]

  // Radioactive:
  type Decay = Frequency
  type Dose = Energy#Div[Mass]

  type Illuminance = Intensity#Div[Area]

  type CatalyticActivity = AmountOfSubstance#Div[Time]
}