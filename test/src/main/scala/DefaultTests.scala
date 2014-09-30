package scunits.test

import scunits._
import scunits.types._
import scunits.default._
import scunits.si._

object DefaultTests {
  // Test Dimless
  implicitly[Dimless#mult[Dimless] =:= Dimless]
  implicitly[Length#mult[Dimless] =:= Length]
  implicitly[Dimless#mult[Length] =:= Length]
  implicitly[Length#div[Dimless] =:= Length]
  implicitly[Dimless#div[Length] =:= Length#neg]

  // Test simple division
  implicitly[Area#div[Length] =:= Length]
  implicitly[Area#div[Area] =:= Dimless]
  implicitly[Inductance#div[Inductance] =:= Dimless]
  implicitly[Force#div[Acceleration] =:= Mass]

  // Test associativity
  implicitly[Area#mult[Length] =:= Length#mult[Area]]
  implicitly[Flux#mult[Power] =:= Power#mult[Flux]]
  implicitly[Time#mult[Mass] =:= Mass#mult[Time]]

  // Length
  implicitly[Area#div[Length] =:= Length]
  implicitly[Volume#div[Length] =:= Area]
  implicitly[Volume#div[Area] =:= Length]
  implicitly[Density#mult[Volume] =:= Mass]
  implicitly[Speed#div[Length] =:= Frequency]
  implicitly[Energy#div[Length] =:= Force]

  // Power and energy
  implicitly[Power#mult[Time] =:= Energy]
  implicitly[Energy#div[Time] =:= Power]
  implicitly[Acceleration#mult[Time] =:= Speed]

  // Electricity
  implicitly[Conductance#mult[Time] =:= Capacitance]
  implicitly[Potential#div[Resistance] =:= Current]
  implicitly[Resistance#mult[Current] =:= Potential]
  implicitly[Conductance#mult[Flux] =:= Charge]
  implicitly[Flux#div[Inductance] =:= Current]

  // Magnetism
  implicitly[Flux#div[Area] =:= FieldStrength]
  implicitly[Flux#div[Time] =:= Potential]
  
  // Radioactivity
  implicitly[Dose#div[Length] =:= Acceleration]
}