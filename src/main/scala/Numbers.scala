package scunits

package object nums {

  object Nat {
    trait Nat {
      type Plus[R <: Nat] <: Nat
    }
    trait _0 extends Nat {
      type Plus[R <: Nat] = R
      type Minus[R <: _0] = _0
    }
    trait Succ[N <: Nat] extends Nat {
      type Plus[R <: Nat] = N#Plus[Succ[R]]
    }

    type +[R <: Nat, L <: Nat] = R#Plus[L]

    // tests
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    implicitly[_1 + _2 =:= _3]
  }

  object Integer {
    import Nat._

    trait Sign {
      type Flip <: Sign
    }
    trait Signed extends Sign
    trait Pos extends Signed {
      type Flip = Neg
    }
    trait Neg extends Signed {
      type Flip = Pos
    }
    trait Neither extends Sign {
      type Flip = Neither
    }

    trait Integer[N <: Nat, S <: Sign] {
      type Nat = N
      type Sign = S
      type Negate = Integer[N, S#Flip]
    }
    trait _0 extends Integer[Nat._0, Neither] {
      type Plus[I <: Integer[_ <: Nat, _ <: Sign]] = I
      type Minus[I <: Integer[_ <: Nat, _ <: Sign]] = I#Negate
    }
    trait Succ[N <: Nat, S <: Signed] extends Integer[N,S] {
      type Plus[I <: Integer[_ <: Nat,S]] = Integer[I#Nat#Plus[N],S]
    }
  }

  // type +[R <: Integer, L <: Integer] = R#Plus[L]

  // tests
  /*type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  implicitly[_1 + _2 =:= _3]*/
}