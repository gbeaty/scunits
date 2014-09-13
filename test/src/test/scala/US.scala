package scunits.test

import scunits._
import scunits.quantity._
import scunits.si._
import scunits.us._

import org.specs2.mutable._

class USUnits extends Specification {
  val err = 0.001
  "US Customary units" should {
    "International lengths" in {
      inch(1) ==== milli(metre, 25.4)
      foot(1) ==== metre(0.3048)
      yard(1) ==== metre(0.9144)
      mile(1) ==== kilo(metre, 1.609344)
      rod(1) ==== metre(5.0292)      
    }
    "Nautical lengths" in {
      fathom(1) ==== metre(1.8288)
      cable(1) ==== metre(219.456)
      nauticalMile(1) ==== metre(1852)
    }
    "Area" in {
      // Survey.squareFoot(1) ==== squareMetre(0.092903412)
      acre(1) ==== squareMetre(4046.8564224)
    }
    "Fluid volume" in {
      import Fluid._
      minim(1) ==== litre(0.000061611519921875)
      dram(1) ==== milli(litre, 3.6966911953125)
      teaspoon(1) ==== milli(litre, 4.92892159375)
      tablespoon(1) ==== milli(litre, 14.78676478125)
      ounce(1) ==== milli(litre, 29.5735295625)
      shot(1) ==== cubicMetre(0.00004436029434375)
      gill(1) ==== milli(litre, 118.29411825)
      cup(1) ==== milli(litre, 236.5882365)
      pint(1) ==== milli(litre, 473.176473)
      quart(1) ==== litre(0.946352946)
      gallon(1) ==== litre(3.785411784)
      barrel(1) ==== cubicMetre(0.119240471196)
      oilBarrel(1) ==== cubicMetre(0.158987294928)
      hogshead(1) ==== cubicMetre(0.238480942392)
    }
    "Dry volume" in {
      import Dry._
      pint(1) ==== litre(0.5506104713575)
      quart(1) ==== litre(1.101220942715)
      gallon(1) ==== litre(4.40488377086)
      peck(1) ==== litre(8.80976754172)
      // bushel(1) ==== litre(35.239070167)
      barrel(1) ==== litre(115.627)
    }
    "temperature" in {
      fahrenheit(-459.67) ==== kelvin(0)
      //fahrenheit(212) ==== celsius(100)
      //fahrenheit(32) ==== celsius(0)
    }
  }
}