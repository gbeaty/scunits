package scunits

package object integer {

  trait Integer {
    type Self <: Integer
    type Succ <: Integer
    type Add[N <: Integer] <: Integer
    type Pred <: Integer
    type Sub[N <: Integer] <: Integer
    type Neg <: Integer
    type Mult[R <: Integer] <: Integer

    type IfZero[B,T <: B,E[_ <: NonZeroInt] <: B] <: B
  }

  trait NonNegInt extends Integer {
    override type Self <: NonNegInt
  }
  trait NonPosInt extends Integer {
    override type Self <: NonPosInt
  }
  trait NonZeroInt extends Integer {
    override type Self <: NonZeroInt
    type IfZero[B,T <: B,E[_ <: NonZeroInt] <: B] = E[Self]
  }
  trait NegInt extends NonPosInt with NonZeroInt {
    override type Self <: NegInt
  }
  trait PosInt extends NonNegInt with NonZeroInt{
    override type Self <: PosInt
  }

  class SuccInt[P <: NonNegInt] extends PosInt {
    type Self = SuccInt[P]
    type Succ = SuccInt[SuccInt[P]]
    type Add[N <: Integer] = P#Add[N#Succ]
    type Pred = P
    type Sub[N <: Integer] = P#Sub[N#Pred]
    type Neg = P#Neg#Pred
    type Mult[R <: Integer] = R#Add[Pred#Mult[R]]
  }

  class PredInt[S <: NonPosInt] extends NegInt {
    type Self = PredInt[S]
    type Succ = S
    type Add[N <: Integer] = S#Add[N#Pred]
    type Pred = PredInt[PredInt[S]]
    type Sub[N <: Integer] = S#Sub[N#Succ]
    type Neg = S#Neg#Succ
    type Mult[R <: Integer] = R#Sub[Succ#Mult[R]]
  }

  final class _0 extends NonNegInt with NonPosInt {
    type Self = _0
    type Succ = SuccInt[_0]
    type Add[N <: Integer] = N
    type Pred = PredInt[_0]
    type Sub[N <: Integer] = N#Neg
    type Neg = _0
    type Mult[R <: Integer] = _0
    type IfZero[B,T <: B,E[_ <: NonZeroInt] <: B] = T
  }

  object Ops {
    type +[L <: Integer, R <: Integer] = L#Add[R]
    type -[L <: Integer, R <: Integer] = L#Sub[R]
    type *[L <: Integer, R <: Integer] = L#Mult[R]
  }

  type p1 = _0#Succ
  type p2 = p1#Succ
  type p3 = p2#Succ
  type p4 = p3#Succ

  type n1 = _0#Pred
  type n2 = n1#Pred
  type n3 = n2#Pred
  type n4 = n3#Pred
}