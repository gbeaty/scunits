package scunits.test

import scunits._

object SIQuantitiesTests {

  // Length
  implicitly[siBaseQuantities.div[Area, Length] =:= Length]
  implicitly[siBaseQuantities.div[Volume, Length] =:= Area]
  implicitly[siBaseQuantities.div[Volume, Area] =:= Length]
  implicitly[siBaseQuantities.mult[Density, Volume] =:= Mass]
  implicitly[siBaseQuantities.div[Speed, Length] =:= Frequency]
  implicitly[siBaseQuantities.div[Energy, Length] =:= Force]

  // Power and energy
  implicitly[siBaseQuantities.mult[Power, Time] =:= Energy]
  implicitly[siBaseQuantities.div[Energy, Time] =:= Power]
  implicitly[siBaseQuantities.mult[Acceleration, Time] =:= Speed]

  // Electricity
  implicitly[siBaseQuantities.mult[Conductance, Time] =:= Capacitance]
  implicitly[siBaseQuantities.div[Voltage, Resistance] =:= Current]
  implicitly[siBaseQuantities.mult[Resistance, Current] =:= Voltage]
  implicitly[siBaseQuantities.mult[Conductance, Flux] =:= Charge]
  implicitly[siBaseQuantities.div[Flux, Inductance] =:= Current]

  // Magnetism
  implicitly[siBaseQuantities.div[Flux, Area] =:= FieldStrength]
  implicitly[siBaseQuantities.div[Flux, Time] =:= Voltage]
  
  // Radioactivity
  implicitly[siBaseQuantities.div[Dose, Length] =:= Acceleration]
}