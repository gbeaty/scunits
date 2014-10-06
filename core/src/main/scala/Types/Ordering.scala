package scunits.types

import scunits._

trait Quant
object Quant {
  trait A extends Quant
  trait B extends Quant
  trait C extends Quant

  trait X extends Quant
  trait Y extends Quant
  trait Z extends Quant
}

trait Comp
trait Lesser extends Comp
trait Equal extends Comp
trait Greater extends Comp

trait QuantOrder {
  type all <: QuantOrder
  type empty <: Bool

  type has[Q >: all <: QuantOrder] = True

  //type compare[R >: all <: QuantOrder] <: Comp // = comp[R]
  // protected type comp[R <: QuantOrder] <: Comp
  // protected type compNel[L <: OrderedQuant] <: Comp  

  type compare[L >: all <: QuantOrder, R >: all <: QuantOrder] = L#comp[R]
  protected type comp[R <: QuantOrder] <: Comp
  protected type compNel[L <: OrderedQuant] <: Comp
}
trait OrderedQuant extends QuantOrder {
  type head <: Quant
  type tail <: QuantOrder
  type empty = False
}
trait LessThan[T <: QuantOrder] extends OrderedQuant {
  type tail = T
  type self = LessThan[T]
  
  type all = this.type with tail#all
  protected type comp[R <: QuantOrder] = R#compNel[self]
  protected type compNel[L <: OrderedQuant] = L#tail#comp[tail]
}
trait Inf extends QuantOrder {
  type all = Inf
  type empty = True

  protected type comp[R <: QuantOrder] = R#empty#branch[Comp,Equal,Lesser]
  protected type compNel[L <: OrderedQuant] = Greater
}

object Test {
  object C extends LessThan[Inf]
  object B extends LessThan[C.type]
  object A extends LessThan[B.type]
  type C = C.type
  type B = B.type
  type A = A.type

  object Z extends LessThan[Inf]
  object Y extends LessThan[Z.type]
  object X extends LessThan[Y.type]
  type Z = Z.type
  type Y = Y.type
  type X = X.type

  type c1 = A#compare[A,B]
}