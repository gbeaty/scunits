package scunits.system

import scunits._
import scunits.default._

trait Planck {
  // Base units:
  val length = UnitM[Length]("planck length","L",1.61619997e-35)
  val mass = UnitM[Mass]("planck mass","M",2.1765113e-5)
  val time = UnitM[Time]("planck planck time","T",5.3910632e-44)
  val charge = UnitM[Charge]("planck charge","Q",1.87554595641e-18)
  val temperature = UnitM[Temperature]("planck temperature","Θ",1.41683385e32)

  // Derived units:
  val area = (length * length).label("planck area","L2")
  val volume = (area * length).label("planck volume","L3")
  val speed = (length / time).label("planck speed","LT-1")
  val momentum = (speed * mass).label("planck momentum","LMT-1")
  val energy = (speed * speed * mass).label("planck energy","L2MT-2")
  val acceleration = (speed / time).label("planck acceleration","LT-2")
  val force = (acceleration / mass).label("planck force","LMT-2")
  val power = (force * speed).label("planck power","L2MT-3")
  val density = (mass / volume).label("planck density","L-3M")
  val energyDensity = (energy / volume).label("planck energy density","L-3M")
  val intensity = (power / area).label("planck intensity","MT-3")
  val angularFrequency = (coef / time).label("planck frequency","T-1")
  val pressure = (area / force).label("planck pressure","L-1MT-2")
  val current = (charge / time).label("planck current","QT-1")
  val voltage = (power / current).label("planck voltage","L2MT-2Q-1")
  val impedance = (voltage / current).label("planck impedance","L2MT-1Q-2")
}