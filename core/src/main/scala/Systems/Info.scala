package scunits.system.info

import scala.math._

import scunits._

trait Bits {
  val bit = UnitM[scunits.Info]("bit","b")
  val byte = (bit * 8).label("byte","B")

  val bps = UnitM[Bandwidth]("bit per second", "bps")
  val Bps = (bps / 8).label("byte per second", "Bps")
}

trait Prefix {
  object IEC {
    val kibi = new UnitPrefix("kibi","Ki",1024)
    val mebi = new UnitPrefix("mebi","Mi",pow(1024,2))
    val gibi = new UnitPrefix("gibi","Gi",pow(1024,3))
    val tebi = new UnitPrefix("tebi","Ti",pow(1024,4))
    val pebi = new UnitPrefix("pebi","Pi",pow(1024,5))
    val exbi = new UnitPrefix("exbi","Ei",pow(1024,6))
    val zebi = new UnitPrefix("zebi","Zi",pow(1024,7))
    val yobi = new UnitPrefix("yobi","Yi",pow(1024,8))
  }
  object Customary {
    val kilo = new UnitPrefix("kilo","k",1024)
    val mega = new UnitPrefix("mega","M",pow(1024,2))
    val giga = new UnitPrefix("giga","G",pow(1024,3))
    val tera = new UnitPrefix("tera","T",pow(1024,4))
    val peta = new UnitPrefix("peta","P",pow(1024,5))
    val exa = new UnitPrefix("exa","E",pow(1024,6))
    val zetta = new UnitPrefix("zetta","Z",pow(1024,7))
    val yotta = new UnitPrefix("yotta","Y",pow(1024,8))
  }
}