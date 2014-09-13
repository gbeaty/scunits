package scunits4

import scunits.BaseQuantityLike
import scunits.integer._
import scunits.integer.Ops._

object DListOps {
  type ^[L <: DList,R <: Integer] = L#Pow[R]
}
import DListOps._
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
  type Quant <: BaseQuantity
  type Exp <: Integer  
}
trait DNelOf[Q <: BaseQuantity] extends DNel {
  type Quant = Q
}
trait DNelTo[E <: Integer] extends DNel {
  type Exp = E
}
trait DNelOfTo[Q <: BaseQuantity, E <: Integer] extends DNelOf[Q] with DNelTo[E]
trait DNelConst[Q <: BaseQuantity, E <: Integer, T <: DList] extends DNelOfTo[Q,E] {
  type Tail = T

  type Neg = DNelConst[Quant,Exp#Neg,Tail#Neg]
  type Pow[R <: Integer] = DNelConst[Quant,Exp#Mult[R],Tail#Pow[R]]
  type Mult[R <: Integer] = DNelConst[Quant,Exp#Add[R],Tail]
  type Append[R <: DList] = DNelConst[Quant,Exp,Tail#Append[R]]
}

trait BaseQuantity {
  type Quant <: BaseQuantity
  type Dim = DNelConst[this.type,_1,DNil]
}

trait Simplifier[I <: DList] {
  type Out <: DList
}
trait SimplifierConst[I <: DList, O <: DList] extends Simplifier[I] {
  type Out = O
}

class Multer  [L <: DList, R <: DList]                                             { type Out <: DList }
class MulterOf[L <: DList, R <: DList, RH <: DNel, O <: DList] extends Multer[L,R] { type Out = O }
trait MulterCombine {
  // Combine L and R, increment L, and reset R to its head (RH).
  // implicit def combine[L <: DNel, R <: DNelOf[L#Quant], RH <: DNel](implicit m: Multer[L#Tail,RH]) =
    // new MulterOf[L,R,RH,m.Out#Append[L#Mult[R#Exp]]]
}

trait MulterSkip extends MulterCombine {
  implicit def zero[L <: DNel, R <: DNelOfTo[L#Quant,L#Exp#Neg], RH <: DNel](implicit m: Multer[L#Tail,RH]) =
    new MulterOf[L,R,RH,m.Out]
}
trait MulterSolve extends MulterSkip {
  implicit def leftNil[R <: DList] = new Multer[DNil,R] { type Out = R }
  implicit def rightNil[L <: DList] = new Multer[L,DNil] { type Out = L }
  implicit val nilNil = new Multer[DNil,DNil] { type Out = DNil }
}
object Multer extends MulterSolve

case class Measure[D <: DList](v: Double) extends AnyVal {
  def ===(r: Measure[D]) = v == r.v
  def *[R <: DList](r: Measure[R])(implicit m: Multer[D,R]) = Measure[m.Out](v * r.v)
  def /[R <: DList](r: Measure[R])(implicit m: Multer[D,R#Neg]) = Measure[m.Out](v / r.v)
}

object Length extends BaseQuantity
object Mass extends BaseQuantity
object Time extends BaseQuantity

object Test {
  import Multer._
  def one[D <: DList] = Measure[D](1)

  type Length = Length.Dim
  type Mass = Mass.Dim
  type Time = Time.Dim

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