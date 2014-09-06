package scunits.unit

import scunits.unit.si.base._

trait Planck {
  val length = (metre * 1.61619997e-35).label("plank length","L")
  val mass = (gram * 2.1765113e-5).label("plank mass","M")
  val time = (second * 5.3910632e-44).label("planck time","T")
  val charge = (coulomb * 1.87554595641e-18).label("plank charge","Q")
  val temperature = (kelvin * 1.41683385e32).label("plank temperature","Θ")
}
package object planck extends Planck