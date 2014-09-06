package scunits.unit.other

import scunits._
import scunits.quantity._
import scunits.unit.si.base._

trait Pressure {
  val mmHg = (pascal * 133.322387415).label("millimeter of mercury", "mm Hg")
  val mmWater = (pascal * 9.80665).label("millimeter of water", "mm H2O")
  val torr = (pascal * (101325.0 / 760.0)).label("torr", "torr")
  val bar = (pascal * 100000.0).label("bar", "bar")
  val at = (pascal * 0.980665e5).label("standard atmosphere", "at")
  val atm = (pascal * 1.01325e5).label("technical atmosphere", "atm")  
}

package object pressure extends Pressure

trait Acceleration {
  val gee = (metrePerSecondSquared * 9.80665).label("gee", "ɡ")
}
package object accel extends Acceleration

trait All extends Pressure with Acceleration
package object all extends All