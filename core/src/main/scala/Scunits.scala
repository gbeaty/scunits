import scunits._
import scunits.integer._
import scunits.integer._
import scunits.integer.Ops._

trait LowPriorityImplicits {
  implicit def removeQuantSkip[Q <: Quantity, In <: DNel](implicit rq: RemoveQuant[Q,In#Tail]):
    RemovedQuant[Q,In,rq.Exp,In#Head :: rq.Rem] = null

  implicit def removeQuantNil[Q <: Quantity]: RemovedQuant[Q,Dimless,_0,Dimless] = null

  implicit def multSkip[L <: DNel, R <: DNel](implicit m: Multer[L#Tail,R]):
    Multing[L,R,L#Head :: m.Out] = null

  implicit def multLeftNil[R <: DNel]: Multing[Dimless,R,R] = null
}

package object scunits extends LowPriorityImplicits {
  implicit def ordering[D <: Dims] = new Ordering[Measure[D]] {
    def compare(l: Measure[D], r: Measure[D]) = if(l < r) -1 else if(l > r) 1 else 0
  }

  implicit def additive[A]: Additive[A,A] = null
  
  implicit def removeQuantMatch[Q <: Quantity, In <: DNelOf[Q]]: RemovedQuant[Q,In,In#Head#Exp,In#Tail] = null

  implicit def multAdd[L <: DNel, R <: DNel, RE <: Integer, RR <: Dims]
    (implicit r: RemovedQuant[L#Head#Quant,R,RE,RR], m: Multer[L#Tail,RR]):
      Multing[L, R,
        (L#Head#Exp + RE)#IfZero[
          Dims,
          m.Out,
          ({type Nz[I <: NonZeroInt] = (L#Head#Quant ^ I) :: m.Out})#Nz
        ]
      ] = null

  
  implicit def multRightNil[L <: Dims]: Multing[L,Dimless,L] = null
  
  val coef = UnitM[Dimless]("","")
  implicit def toCoef(d: Double) = Measure[Dimless](d)

  // Base quantities:
  sealed trait Length extends BaseQuantity[Length]
  sealed trait Mass extends BaseQuantity[Mass]
  sealed trait Time extends BaseQuantity[Time]
  sealed trait Temperature extends BaseQuantity[Temperature]
  sealed trait AmountOfSubstance extends BaseQuantity[AmountOfSubstance]
  sealed trait Info extends BaseQuantity[Info]
  sealed trait Current extends BaseQuantity[Current]
  sealed trait Intensity extends BaseQuantity[Intensity]

  // SI base quantities:
  type Frequency = (Time^n1) :: Dimless
  type Force = (Mass^p1) :: (Length^p1) :: (Time^n2) :: Dimless
  type Pressure = (Mass^p1) :: (Length^n1) :: (Time^n2) :: Dimless
  type Energy = (Mass^p1) :: (Length^p2) :: (Time^n2) :: Dimless
  type Power = (Mass^p1) :: (Length^p2) :: (Time^n3) :: Dimless
  type Charge = (Time^p1) :: (Current^p1) :: Dimless
  type Voltage = (Mass^p1) :: (Length^p2) :: (Time^n3) :: (Current^p1) :: Dimless
  type Capacitance = (Mass^p1) :: (Length^n2) :: (Time^p4) :: (Current^p2) :: Dimless
  type Resistance = (Mass^p1) :: (Length^p2) :: (Time^n3) :: (Current^n2) :: Dimless
  type Conductance = Resistance#Neg
  type Flux = (Mass^p1) :: (Length^p2) :: (Time^n2) :: (Current^n1) :: Dimless
  type FieldStrength = (Mass^p1) :: (Time^n2) :: (Current^n1) :: Dimless
  type Inductance = (Mass^p1) :: (Length^p2) :: (Time^n2) :: (Current^n2) :: Dimless
  type Illuminance = (Length^n2) :: (Intensity^p1) :: Dimless
  type Radioactivity = Frequency
  type Dose = (Mass^p2) :: (Time^n2) :: Dimless
  type ReactionRate = (Time^n1) :: (AmountOfSubstance^p1) :: Dimless

  // Derived quantities:
  type Area = (Length^p2) :: Dimless
  type Volume = (Length^p3) :: Dimless
  type Speed = (Length^p1) :: (Time^n1) :: Dimless
  type VolumetricFlow = (Length^p2) :: (Time^n1) :: Dimless
  type Acceleration = (Length^p1) :: (Time^n2) :: Dimless
  type Jerk = (Length^p1) :: (Time^n3) :: Dimless
  type Jounce = (Length^p1) :: (Time^n4) :: Dimless  
  type Momentum = (Length^p1) :: (Mass^p1) :: (Time^n1) :: Dimless
  type AngularMomentum = (Length^p2) :: (Mass^p1) :: (Time^n1) :: Dimless
  type Torque = (Length^p2) :: (Mass^p1) :: (Time^n2) :: Dimless
  // type Yank = 

  // Other:
  type InfoRate = (Info^p1) :: (Time^n1) :: Dimless
}