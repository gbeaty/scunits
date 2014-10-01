package scunits.system.other

import scunits._
import scunits.default._

trait Pressure {
  val mmHg = UnitM[scunits.default.Pressure]("millimeter of mercury", "mm Hg", 133.322387415)
  val mmWater = UnitM[scunits.default.Pressure]("millimeter of water", "mm H2O", 9.80665)
  val torr = UnitM[scunits.default.Pressure]("torr", "torr",101325.0 / 760.0)
  val bar = UnitM[scunits.default.Pressure]("bar", "bar", 100000.0)
  val at = UnitM[scunits.default.Pressure]("standard atmosphere", "at", 0.980665e5)
  val atm = UnitM[scunits.default.Pressure]("technical atmosphere", "atm", 1.01325e5)
}

trait Acceleration {
  val gee = UnitM[scunits.default.Acceleration]("gee", "ɡ", 9.80665)
}