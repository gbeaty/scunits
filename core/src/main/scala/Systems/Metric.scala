package scunits.unit

import scunits._
import scunits.quantity._
import scunits.unit.si._
import scunits.unit.si.Prefix._

package object metric {
  val litre = (cubicMetre / 1000).label("litre","L")

  // val kmpl = kilo(metre).div("kilometers per liter", "km/L", litre)
}