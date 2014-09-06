package scunits.system.other

import scunits._

trait Pressure {
  val mmHg = UnitM[quantity.Pressure]("millimeter of mercury", "mm Hg", 133.322387415)
  val mmWater = UnitM[quantity.Pressure]("millimeter of water", "mm H2O", 9.80665)
  val torr = UnitM[quantity.Pressure]("torr", "torr",101325.0 / 760.0)
  val bar = UnitM[quantity.Pressure]("bar", "bar", 100000.0)
  val at = UnitM[quantity.Pressure]("standard atmosphere", "at", 0.980665e5)
  val atm = UnitM[quantity.Pressure]("technical atmosphere", "atm", 1.01325e5)
}

trait Acceleration {
  val gee = UnitM[quantity.Acceleration]("gee", "ɡ", 9.80665)
}

trait All extends Pressure with Acceleration