package scunits.typeclass

import scunits.BaseQuantityLike
import scunits.integer._
import scunits.integer.Ops._

trait Dim {  
  type Quant <: Quantity
  type Exp <: Integer
  type Nel = DNelConst[Quant,Exp,DNil]
}
trait DimOf[Q <: Quantity] extends Dim {
  type Quant = Q
}
trait ^[L <: Quantity, R <: Integer] extends DimOf[L] {
  type Exp = R
}
trait DList {
  type Neg <: DList
  type Pow[R <: Integer] <: DList
  type Mult[R <: Integer] <: DList
  type Append[R <: DList] <: DList
}
trait DNil extends DList {
  type Neg = DNil
  type Pow[R <: Integer] = DNil
  type Mult[R <: Integer] = DNil
  type Append[R <: DList] = R
}
trait DNel extends DList {
  type Tail <: DList
  type Quant <: Quantity
  type Exp <: Integer
  type Head = Quant ^ Exp
}
trait DNelOf[Q <: Quantity] extends DNel {
  type Quant = Q
}
trait DNelTo[E <: Integer] extends DNel {
  type Exp = E
}
trait DNelOfTo[Q <: Quantity, E <: Integer] extends DNelOf[Q] with DNelTo[E]
trait DNelConst[Q <: Quantity, E <: Integer, T <: DList] extends DNelOfTo[Q,E] {
  type Tail = T

  type Neg = DNelConst[Quant,Exp#Neg,Tail#Neg]
  type Pow[R <: Integer] = DNelConst[Quant,Exp#Mult[R],Tail#Pow[R]]
  type Mult[R <: Integer] = DNelConst[Quant,Exp#Add[R],Tail]
  type Append[R <: DList] = DNelConst[Quant,Exp,Tail#Append[R]]
}

trait Quantity extends Dim
trait BaseQuantity[Q <: BaseQuantity[Q]] extends Quantity with (Q ^ _1) with DNelConst[Q,_1,DNil] {
  override type Quant = Q
  override type Exp = _1
}

trait Simplifier[I <: DList] {
  type Out <: DList
}
trait SimplifierConst[I <: DList, O <: DList] extends Simplifier[I] {
  type Out = O
}

class RemoveDim[D <: Dim, In <: DList] {
  type Rem <: DList
}

class RemovedDim[D <: Dim, In <: DList, R <: DList] extends RemoveDim[D,In] {
  type Rem = R
}
trait RemoveDimOps {
  implicit def removeDimSkip[D <: Dim, In <: DNel, R <: DList](implicit h: RemovedDim[D,In#Tail,R]):
    RemovedDim[D,In,DNelConst[In#Quant,In#Exp,R]] = null
  implicit def removeDimMatch[D <: Dim, In <: DNelOfTo[D#Quant, D#Exp]]: RemovedDim[D,In,In#Tail] = null
}

class HasAll[L <: DList, In <: DList]
trait HasAllOps {
  implicit def hasAllMatch[L <: DNel, In <: DList](implicit h: RemoveDim[L#Head,In], ha: HasAll[L#Tail,In]): HasAll[L,In] = null
  implicit def hasAllEnd[In <: DList]: HasAll[DNil,In] = null
}

class RemoveQuant[Q <: Quantity, In <: DList] {
  type Exp <: Integer
  type Rem <: DList
}
class RemovedQuant[Q <: Quantity, In <: DList, E <: Integer, R <: DList] extends RemoveQuant[Q,In] {
  type Exp = E
  type Rem = R
}
trait RemoveQuantSkip {
  implicit def removeQuantSkip[Q <: Quantity, In <: DNel](implicit rq: RemoveQuant[Q,In#Tail]):
    RemovedQuant[Q,In,rq.Exp,DNelConst[In#Quant,In#Exp,rq.Rem]] = null
}
trait RemoveQuantOps extends RemoveQuantSkip {
  implicit def removeQuantNil[Q <: Quantity]: RemovedQuant[Q,DNil,_0,DNil] = null
  implicit def removeQuantMatch[Q <: Quantity, In <: DNelOf[Q]]: RemovedQuant[Q,In,In#Exp,In#Tail] = null
}

class Adder[L <: DList, R <: DList]
trait AdderOps {
  implicit def adderMatch[L <: DNel, R <: DNel](implicit lr: HasAll[L,R], rl: HasAll[R,L]): Adder[L,R] = null
  implicit val adderNil: Adder[DNil,DNil] = null
}

class IsZero[I <: Integer]
class Zero[I <: Integer] extends IsZero[I]
class NotZero[I <: Integer] extends IsZero[I]
trait IsZeroOps {
  implicit val isZero: Zero[_0] = null
  implicit def notZero[I <: NonZeroInt]: NotZero[I] = null
}

class Multer[L <: DList, R <: DList] { type Out <: DList }
class Multing[L <: DList, R <: DList, O <: DList] extends Multer[L,R] {
  type Out = O
}
trait MultSkip {
  implicit def multSkip[L <: DNel, R <: DNel](implicit m: Multer[L#Tail,R]):
    Multing[L,R,DNelConst[L#Quant, L#Exp, m.Out]] = null
}
trait MultMatch extends MultSkip {
  implicit def multAdd[L <: DNel, R <: DNel, RE <: Integer, RR <: DList]
    (implicit r: RemovedQuant[L#Quant,R,RE,RR], m: Multer[L#Tail,RR], iz: NotZero[L#Exp+RE]):
      Multing[L, R, DNelConst[L#Quant, L#Exp + RE, m.Out]] = null

  implicit def multCancel[L <: DNel, R <: DNel, RE <: Integer, RR <: DList]
    (implicit r: RemovedQuant[L#Quant,R,RE,RR], m: Multer[L#Tail,RR], iz: Zero[L#Exp+RE]):
      Multing[L, R, m.Out] = null
}
trait MultNil extends MultMatch {
  implicit def multLeftNil[R <: DNel]: Multing[DNil,R,R] = null
  implicit def multRightNil[L <: DNel]: Multing[L,DNil,L] = null
}
trait MulterOps extends MultNil {
  implicit val multNil: Multing[DNil,DNil,DNil] = null
}

object DListOps extends RemoveDimOps with HasAllOps with MulterOps with AdderOps with RemoveQuantOps with IsZeroOps

case class TCMeasure[D <: DList](v: Double) extends AnyVal {
  type Dims = D
  def ===(r: TCMeasure[D]) = v == r.v
  def *[R <: DList](r: TCMeasure[R])(implicit m: Multer[D,R]) = TCMeasure[m.Out](v * r.v)
  def /[R <: DList](r: TCMeasure[R])(implicit m: Multer[D,R#Neg]) = TCMeasure[m.Out](v / r.v)

  def +[R <: DList](r: TCMeasure[R])(implicit a: Adder[D,R]) = TCMeasure[D](v + r.v)
  def -[R <: DList](r: TCMeasure[R])(implicit a: Adder[D,R]) = TCMeasure[D](v - r.v)

  def ×(r: Double) = TCMeasure[D](v * r)
  def ÷(r: Double) = TCMeasure[D](v / r)
}

class Length extends BaseQuantity[Length]
class Mass extends BaseQuantity[Mass]
class Time extends BaseQuantity[Time]

object Test {
  import DListOps._
  def remDim[M <: Dim, In <: DList](implicit i: RemoveDim[M,In]) = new RemovedDim[M,In,i.Rem]
  def hasAll[M <: DList, In <: DList](implicit i: HasAll[M, In]) = i
  def remQuant[Q <: Quantity, In <: DList](implicit rq: RemoveQuant[Q,In]) = new RemovedQuant[Q,In,rq.Exp,rq.Rem]
  def m[D <: DList] = TCMeasure[D](1)
  def z[I <: Integer](implicit z: Zero[I]) = z
  def nz[I <: Integer](implicit nz: NotZero[I]) = nz

  type ::[L <: Dim, R <: DList] = DNelConst[L#Quant, L#Exp, R]

  // remDim tests:
  remDim[Length, Length]: RemovedDim[Length,Length,DNil]
  remDim[Length, Length :: Mass :: DNil]: RemovedDim[Length, Length :: Mass :: DNil, Mass :: DNil]
  remDim[Length, Mass :: Length :: DNil]: RemovedDim[Length, Mass :: Length :: DNil, Mass :: DNil]
  // Should not compile:
  // remDim[Time, DNil]
  // remDim[Time, Mass :: Length :: DNil]

  // HasAll tests:
  hasAll[DNil,DNil]
  hasAll[Length,Length]
  hasAll[Length, Length :: Mass :: DNil]
  hasAll[Length, Mass :: Length :: DNil]
  hasAll[Length :: Mass :: DNil, Length :: Mass :: DNil]
  hasAll[Mass :: Length :: DNil, Length :: Mass :: DNil]
  hasAll[Mass :: Length :: DNil, Length :: Time :: Mass :: DNil]
  // Should not compile:
  // hasAll[Time :: Length :: DNil, Length :: Mass :: DNil]

  // RemoveQuant tests:
  remQuant[Length,Length]: RemovedQuant[Length,Length,_1,DNil]
  remQuant[Length, Length :: Mass :: DNil]: RemovedQuant[Length, Length :: Mass :: DNil, _1, Mass :: DNil]
  remQuant[Length, Mass :: Length :: DNil]: RemovedQuant[Length, Mass :: Length :: DNil, _1, Mass :: DNil]
  remQuant[Time, Mass :: Length :: DNil]: RemovedQuant[Time, Mass :: Length :: DNil, _0, Mass :: Length :: DNil]

  // IsZero tests:
  z[_0]
  nz[_1#Neg]
  nz[_5]
  // Should not compile:
  // nz[_0]
  // z[_2]
  // z[_3#Neg]


  m[DNil] + m[DNil]  
  m[Mass :: Length :: DNil] + m[Length :: Mass :: DNil]
  // m[Time :: Length :: DNil] + m[Length :: Mass :: DNil]

  m[DNil] * m[Length]: TCMeasure[Length]
  m[Length] * m[DNil]: TCMeasure[Length]
  m[Length] * m[Length]: TCMeasure[(Length^_2) :: DNil]
  m[Length :: Mass :: DNil] * m[Length]: TCMeasure[(Length^_2) :: Mass :: DNil]
  m[Length :: Mass :: DNil] * m[Mass]: TCMeasure[Length :: (Mass^_2) :: DNil]
  m[Length :: Mass :: DNil] * m[Length :: Mass :: DNil]: TCMeasure[(Length^_2) :: (Mass^_2) :: DNil]
  m[Length :: DNil] * m[Mass :: DNil]: TCMeasure[Length :: Mass :: DNil]
  m[Mass :: Length :: DNil] * m[Length :: Mass :: Time :: DNil]: TCMeasure[(Mass^_2) :: (Length^_2) :: Time :: DNil]

  m[Length] / m[DNil]: TCMeasure[Length]
  m[DNil]./(m[Length])(multLeftNil): TCMeasure[(Length^_1#Neg) :: DNil]
  m[Length] / m[Length]: TCMeasure[DNil]
}