package scunits.unit

import scunits._
import scunits.quantity._
import scunits.unit.si._
import scunits.unit.si.Prefix._

package object metric {
  val litre = MultUnitM[Volume]("litre","L",0.001)

  // val kmpl = kilo(metre).div("kilometers per liter", "km/L", litre)
}