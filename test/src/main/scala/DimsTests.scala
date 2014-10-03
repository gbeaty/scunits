package scunits

import scunits.integer._
import scunits.integer.Ops._

object DimensionTypeclasses {

  def remQuant[Q <: Quantity, In <: Dims](implicit rq: RemoveQuant[Q,In]) = new RemovedQuant[Q,In,rq.Exp,rq.Rem]
  def m[D <: Dims] = Measure[D](1)

  // RemoveQuant tests:
  remQuant[Length, (Length^p1) :: Dimless]: RemovedQuant[Length,(Length^p1) :: Dimless,p1,Dimless]
  remQuant[Length, (Length^p1) :: (Mass^p1) :: Dimless]:
    RemovedQuant[Length, (Length^p1) :: (Mass^p1) :: Dimless, p1, (Mass^p1) :: Dimless] 
  remQuant[Length, (Mass^p1) :: (Length^p1) :: Dimless]:
    RemovedQuant[Length, (Mass^p1) :: (Length^p1) :: Dimless, p1, (Mass^p1) :: Dimless]
  remQuant[Time, Dimless]: RemovedQuant[Time, Dimless, _0, Dimless]
  remQuant[Time, (Length^p1) :: Dimless]: RemovedQuant[Time, (Length^p1) :: Dimless, _0, (Length^p1) :: Dimless]
  remQuant[Time, (Mass^p1) :: Dimless]: RemovedQuant[Time, (Mass^p1) :: Dimless, _0, (Mass^p1) :: Dimless]  
  
  // Additive tests:
  m[Dimless] + m[Dimless]
  m[(Length^p1) :: (Mass^p1) :: Dimless] + m[(Length^p1) :: (Mass^p1) :: Dimless]
  m[(Mass^p1) :: (Length^p1) :: Dimless] + m[(Length^p1) :: (Mass^p1) :: Dimless]
  m[(Length^p1) :: (Mass^p1) :: Dimless] - m[(Length^p1) :: (Mass^p1) :: Dimless]
  m[(Mass^p1) :: (Length^p1) :: Dimless] - m[(Length^p1) :: (Mass^p1) :: Dimless]
  // Should not compile:
  // m[Time :: Length :: Dimless] + m[Length :: Mass :: Dimless]
  // m[Length :: Mass :: Dimless] + m[Length :: Dimless]

  // Div tests:
  m[(Length^p1) :: Dimless] / m[Dimless]: Measure[(Length^p1) :: Dimless]
  m[Dimless] / m[(Length^p1) :: Dimless]: Measure[(Length^p1#Neg) :: Dimless]
  m[Length] / m[Length]: Measure[Dimless]

  // Mult tests:
  m[Dimless] * m[Dimless]: Measure[Dimless]
  m[Dimless] * m[Length]: Measure[Length]
  m[Dimless] * m[Length#Neg]: Measure[Length#Neg]
  m[Length] * m[Dimless]: Measure[Length]
  m[Length] * m[Length]: Measure[(Length^p2) :: Dimless]
  m[(Length^p1) :: (Mass^p1) :: Dimless] * m[Length]: Measure[(Length^p2) :: (Mass^p1) :: Dimless]
  m[(Length^p1) :: (Mass^p1) :: Dimless] * m[Mass]: Measure[(Length^p1) :: (Mass^p2) :: Dimless]
  m[(Length^p1) :: (Mass^p1) :: Dimless] * m[(Length^p1) :: (Mass^p1) :: Dimless]:
    Measure[(Length^p2) :: (Mass^p2) :: Dimless]
  m[(Length^p1) :: Dimless] * m[(Mass^p1) :: Dimless]: Measure[(Length^p1) :: (Mass^p1) :: Dimless]
  m[(Mass^p1) :: (Length^p1) :: Dimless] * m[(Length^p1) :: (Mass^p1) :: (Time^p1) :: Dimless]:
    Measure[(Mass^p2) :: (Length^p2) :: (Time^p1) :: Dimless]
}