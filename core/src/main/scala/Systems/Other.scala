package scunits.unit

import scunits._
import scunits.quantity._

package object pressure {
  val pascal = unit.si.pascal
  val mmHg = MultUnitM[Pressure]("millimeter of mercury", "mm Hg", 133.322387415)
  val mmWater = MultUnitM[Pressure]("millimeter of water", "mm H2O", 9.80665)
  val psi = MultUnitM[Pressure]("pounds per square inch", "psi", 6894.757)
  val torr = MultUnitM[Pressure]("torr", "torr",  101325.0 / 760.0)
  val bar = MultUnitM[Pressure]("bar", "bar", 100000.0)
  val at = MultUnitM[Pressure]("standard atmosphere", "at", 0.980665e5)
  val atm = MultUnitM[Pressure]("technical atmosphere", "atm", 1.01325e5)
}

package object power {
  val watt = unit.si.watt
  object hp {
    val i = MultUnitM[Power]("mechanical horsepower", "hp(I)", 745.699872)
    val m = MultUnitM[Power]("metric horsepower", "hp(M)", 735.49875)
    val e = MultUnitM[Power]("electric horsepower", "hp(E)", 746.0)
    val s = MultUnitM[Power]("boiler horsepower", "hp(S)", 9812.5)
  }
  // val dBm = UnitM[Power]("")
}

package object acceleration {
  val gee = MultUnitM[Acceleration]("gee", "g", 9.80665)
}