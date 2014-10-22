package scunits.test

import scunits._
import scunits.siBaseQuantities.ops._

object SIQuantitiesTests {

  // Length
  implicitly[Area / Length =:= Length]
  implicitly[Volume / Length =:= Area]
  implicitly[Volume / Area =:= Length]
  implicitly[Density * Volume =:= Mass]
  implicitly[Speed / Length =:= Frequency]
  implicitly[Energy / Length =:= Force]

  // Power and energy
  implicitly[Power * Time =:= Energy]
  implicitly[Energy / Time =:= Power]
  implicitly[Acceleration * Time =:= Speed]

  // Electricity
  implicitly[Conductance * Time =:= Capacitance]
  implicitly[Voltage / Resistance =:= Current]
  implicitly[Resistance * Current =:= Voltage]
  implicitly[Conductance * Flux =:= Charge]
  implicitly[Flux / Inductance =:= Current]

  // Magnetism
  implicitly[Flux / Area =:= FieldStrength]
  implicitly[Flux / Time =:= Voltage]
  
  // Radioactivity
  implicitly[Dose / Length =:= Acceleration]
}