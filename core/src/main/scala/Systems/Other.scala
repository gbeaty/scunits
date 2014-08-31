package scunits.unit

import scunits._
import scunits.quantity._

package object pressure {
  val pascal = unit.si.pascal
  val mmHg = UnitM[Pressure]("millimeter of mercury", "mm Hg", 133.322387415)
  val mmWater = UnitM[Pressure]("millimeter of water", "mm H2O", 9.80665)
  val psi = UnitM[Pressure]("pounds per square inch", "psi", 6894.757)
  val torr = UnitM[Pressure]("torr", "torr",  101325.0 / 760.0)
  val bar = UnitM[Pressure]("bar", "bar", 100000.0)
  val at = UnitM[Pressure]("standard atmosphere", "at", 0.980665e5)
  val atm = UnitM[Pressure]("technical atmosphere", "atm", 1.01325e5)
}

package object power {
  val watt = unit.si.watt
  object hp {
    val i = UnitM[Power]("mechanical horsepower", "hp(I)", 745.699872)
    val m = UnitM[Power]("metric horsepower", "hp(M)", 735.49875)
    val e = UnitM[Power]("electric horsepower", "hp(E)", 746.0)
    val s = UnitM[Power]("boiler horsepower", "hp(S)", 9812.5)
  }
  // val dBm = UnitM[Power]("")
}

package object acceleration {
  val gee = UnitM[Acceleration]("gee", "g", 9.80665)
}