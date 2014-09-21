package scunits.test

import scunits._
import scunits.types._
import scunits.default._

object DefaultTests {
  // Test Dimless
  implicitly[Dimless#Mult[Dimless] =:= Dimless]
  implicitly[Length#Mult[Dimless] =:= Length]
  implicitly[Dimless#Mult[Length] =:= Length]
  implicitly[Length#Div[Dimless] =:= Length]
  implicitly[Dimless#Div[Length] =:= Length#Neg]

  // Test simple division
  implicitly[Area#Div[Length] =:= Length]
  implicitly[Area#Div[Area] =:= Dimless]
  implicitly[Inductance#Div[Inductance] =:= Dimless]
  implicitly[Force#Div[Acceleration] =:= Mass]

  // Test associativity
  implicitly[Area#Mult[Length] =:= Length#Mult[Area]]
  implicitly[Flux#Mult[Power] =:= Power#Mult[Flux]]
  implicitly[Time#Mult[Mass] =:= Mass#Mult[Time]]

  // Length
  implicitly[Area#Div[Length] =:= Length]
  implicitly[Volume#Div[Length] =:= Area]
  implicitly[Volume#Div[Area] =:= Length]
  implicitly[Density#Mult[Volume] =:= Mass]
  implicitly[Speed#Div[Length] =:= Frequency]
  implicitly[Energy#Div[Length] =:= Force]

  // Power and energy
  implicitly[Power#Mult[Time] =:= Energy]
  implicitly[Energy#Div[Time] =:= Power]
  implicitly[Acceleration#Mult[Time] =:= Speed]

  // Electricity
  implicitly[Conductance#Mult[Time] =:= Capacitance]
  implicitly[Potential#Div[Resistance] =:= Current]
  implicitly[Resistance#Mult[Current] =:= Potential]
  implicitly[Conductance#Mult[Flux] =:= Charge]
  implicitly[Flux#Div[Inductance] =:= Current]

  // Magnetism
  implicitly[Flux#Div[Area] =:= FieldStrength]
  implicitly[Flux#Div[Time] =:= Potential]
  
  // Radioactivity
  implicitly[Dose#Div[Length] =:= Acceleration]
}