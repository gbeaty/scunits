package scunits4

import scunits.BaseQuantityLike
import scunits.integer._
import scunits.integer.Ops._

trait Dim {  
  type Quant <: Quantity
  type Exp <: Integer
  type Nel = DNelConst[Quant,Exp,DNil]
}
trait ^[L <: Quantity, R <: Integer] extends Dim {
  type Quant = L
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

class Has[D <: Dim, In <: DList]
trait HasOps {
  implicit def hasSkip[D <: Dim, In <: DNel](implicit h: Has[D,In#Tail]) =
    new Has[D,In]

  implicit def hasMatch[D <: Dim, In <: DNelOfTo[D#Quant, D#Exp]] = new Has[D,In]
}

class HasAll[L <: DList, In <: DList]
trait HasAllOps {
  implicit def hasAllMatch[L <: DNel, In <: DList](implicit h: Has[L#Head,In], ha: HasAll[L#Tail,In]) = new HasAll[L,In]
  implicit def hasAllEnd[In <: DList] = new HasAll[DNil,In]
}

class Adder[L <: DList, R <: DList]
trait AdderOps {
  implicit def adderMatch[L <: DNel, R <: DNel](implicit lr: HasAll[L,R], rl: HasAll[R,L]) =
    new Adder[L,R]
  implicit val adderNil = new Adder[DNil,DNil]
}

class Multer[L <: DList, R <: DList] { type Out <: DList }
class MulterProgress[L <: DList, R <: DList, NR <: DNel] extends Multer[L,R]
trait MulterOps {
  implicit def multMatch[L <: DNel, R <: DNelOf[L#Quant], NR <: DList] = {}

  implicit def multLeftNil[R <: DNel] = new Multer[DNil,R] { type Out = R }
  implicit def multRightNil[L <: DNel] = new Multer[L,DNil] { type Out = L }
  implicit val multNil = new Multer[DNil,DNil] { type Out = DNil }
}

object DListOps extends HasOps with HasAllOps with MulterOps with AdderOps {
  type ^[L <: DList,R <: Integer] = L#Pow[R]
}

case class Measure[D <: DList](v: Double) extends AnyVal {
  def ===(r: Measure[D]) = v == r.v
  def *[R <: DList](r: Measure[R])(implicit m: Multer[D,R]) = Measure[m.Out](v * r.v)
  def /[R <: DList](r: Measure[R])(implicit m: Multer[D,R#Neg]) = Measure[m.Out](v / r.v)

  def +[R <: DList](r: Measure[R])(implicit a: Adder[D,R]) = Measure[D](v + r.v)
  def -[R <: DList](r: Measure[R])(implicit a: Adder[D,R]) = Measure[D](v - r.v)
}

class Length extends BaseQuantity[Length]
class Mass extends BaseQuantity[Mass]
class Time extends BaseQuantity[Time]

object Test {
  import DListOps._
  def has[M <: Dim, In <: DList](implicit c: Has[M, In]) {}
  def hasAll[M <: DList, In <: DList](implicit c: HasAll[M, In]) {}
  def one[D <: DList] = Measure[D](1)

  type ::[L <: DNel, R <: DList] = DNelConst[L#Quant, L#Exp, R]

  // Has tests:
  has[Length, Length]
  has[Length, Length :: Mass :: DNil]
  has[Length, Mass :: Length :: DNil]
  // has[Time, DNil]
  // has[Time, Mass :: Length :: DNil]

  // HasAll tests:
  hasAll[DNil,DNil]
  hasAll[Length,Length]
  hasAll[Length, Length :: Mass :: DNil]
  hasAll[Length, Mass :: Length :: DNil]
  hasAll[Length :: Mass :: DNil, Length :: Mass :: DNil]
  hasAll[Mass :: Length :: DNil, Length :: Mass :: DNil]
  hasAll[Mass :: Length :: DNil, Length :: Time :: Mass :: DNil]
  // hasAll[Time :: Length :: DNil, Length :: Mass :: DNil]

  one[DNil] + one[DNil]
  one[Length] + one[Length]
  one[Mass :: Length :: DNil] + one[Length :: Mass :: DNil]
  // one[Time :: Length :: DNil] + one[Length :: Mass :: DNil]

  one[DNil] * one[DNil]: Measure[DNil]
  one[Length] * one[DNil]: Measure[Length]
  one[DNil] * one[Length]: Measure[Length]
  // one[Length]./(one[Length])(zero[Length,Length#Neg,Length#Neg]): Measure[DNil]

  // MulterOf[DNelConst[Length.type,_1,DNil],DNelConst[Length.type,_1,DNil],DNelConst[Length.type,_1,DNil],DNelConst[Length.type,_1,DNelConst[Length.type,_2,DNil]]]
  // Multer[Test.Length,Test.Length#Neg]
  // Multer[DNelConst[Length.type,_1,DNil],DNelConst[Length.type,_1#Neg,DNil]]
  /*MulterOf[
    DNelConst[Length.type,_1,DNil],
    DNelConst[Length.type, _1#Neg, DNil],
    DNil
  ]
  // Req:
  Multer[
    DNelConst[Length.type,_1,DNil],
    DNelConst[Length.type,_1#Neg,DNil]
  ]*/
}