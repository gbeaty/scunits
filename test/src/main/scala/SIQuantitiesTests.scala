package scunits.test

import scunits._

object SIQuantitiesTests {

  // Length
  implicitly[Area#op[Length]#div =:= Length]
  implicitly[Volume#op[Length]#div =:= Area]
  implicitly[Volume#op[Area]#div =:= Length]
  implicitly[Density#op[Volume]#mult =:= Mass]
  implicitly[Speed#op[Length]#div =:= Frequency]
  implicitly[Energy#op[Length]#div =:= Force]

  // Power and energy
  implicitly[Power#op[Time]#mult =:= Energy]
  implicitly[Energy#op[Time]#div =:= Power]
  implicitly[Acceleration#op[Time]#mult =:= Speed]

  // Electricity
  implicitly[Conductance#op[Time]#mult =:= Capacitance]
  implicitly[Voltage#op[Resistance]#div =:= Current]
  implicitly[Resistance#op[Current]#mult =:= Voltage]
  implicitly[Conductance#op[Flux]#mult =:= Charge]
  implicitly[Flux#op[Inductance]#div =:= Current]

  // Magnetism
  implicitly[Flux#op[Area]#div =:= FieldStrength]
  implicitly[Flux#op[Time]#div =:= Voltage]
  
  // Radioactivity
  implicitly[Dose#op[Length]#div =:= Acceleration]
}