package scunits

import scunits.types._

object ScalarTests {
  // Scalar[Length](1)./(Scalar[Time](1))(siBaseQuantities): Scalar[Speed]

  import scunits.planck._
  // (length.**(length)(siBaseQuantities))//: UnitM[Area]
  // siBaseQuantities: QListOf[Length]
  type Qs = Basis.Length :: QNil
  val qs = new Qs
  val nqs = new (Basis.Time :: Basis.Mass :: QNil)

  // implicitly[QListOf[Basis.Length with Dims] <:< Qs]
  // implicitly[Qs#dims <:< SiBaseQuantities]
  // implicitly[Qs#dims <:< SiBaseQuantities#dims]
  implicitly[Dims <:< Any]
  implicitly[SiBaseQuantities#dims <:< Qs#dims]  
  implicitly[Qs <:< QListOf[Length]]
  // implicitly[SiBaseQuantities#dims <:< QListOf[Length]#dims]
  
  /*trait A[E <: Integer] { type a = E }
  trait B[E <: Integer] { type b = E }
  trait C[E <: Integer] { type c = E }
  type AB = A[_] with B[_]
  type ABC = AB with C[_]
  type A1 = A[p1]
  type B1 = B[p2]
  type C1 = C[p3]
  implicitly[A1 <:< AB]*/

  trait A { type a <: Integer }
  trait B { type b <: Integer }
  trait C { type c <: Integer }
  type AB = A with B
  type ABC = AB with C
  type A1 = A { type a = p1 }
  type B2 = B { type b = p2 }
  type C3 = C { type c = p3 }
  implicitly[AB with A1 <:< AB]
  implicitly[AB with C3 <:< AB]

  // Should compile:
  Scalar[Length](1).pow(new p2)(qs): Scalar[Area]
  Scalar[Length](1).pow(new p2)(siBaseQuantities): Scalar[Area]
  Scalar[Length](1).pow(new p2): Scalar[Area]

  // Should not compile:
  Scalar[Length](1).pow(new p2)(nqs)
}