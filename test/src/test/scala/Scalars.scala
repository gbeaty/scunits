package scunits.test

import scunits._
import scunits.si._

import org.specs2.mutable._

class Scalars extends Specification {
  val m1 = metre(1.0)
  val m2 = metre(2.0)
  "Scalars" should {    
    "Add" in {
      m1 + m2 ==== metre(3.0)
    }
    "Subtract" in {
      m2 - m1 ==== m1
    }
    "Multiply" in {
      m1 * m2 ==== squareMetre(2.0)
    }
    "Divide" in {
      m1 / m2 ==== coef(0.5)
    }
    "Automatically invert" in {
      m2.inv ==== m2
      second(2.0).inv ==== hertz(0.5)
    }
    "Compare" in {
      m1 === m1
      m2 must be_> (m1)
      m1 must be_< (m2)
    }
  }

  /*"Abstract measures" should {    
    "Add" in {
      def add[L <: Dims](l: Scalar[L], r: Scalar[L]): Scalar[L] = l + r
      add(m1,m2) ==== metre(3.0)
    }
    "Subtract" in {
      def sub[L <: Dims](l: Scalar[L], r: Scalar[L]): Scalar[L] = l - r
      sub(m1,m2) ==== metre(-1.0)
    }
    "Multiply" in {
      "Cancel denominators" in {
        def mult[L <: Dims, R <: Dims](l: Scalar[L], r: Scalar[R#Div[L]]): Scalar[R] = l * r
        mult[Time,Length](second(2.0),metrePerSecond(2.0)) ==== metre(4.0)

        def mult2[L <: Dims, R <: Dims](l: Scalar[L#Div[R]], r: Scalar[R]): Scalar[L] = l * r
        mult2[Length,Time](metrePerSecond(2.0),second(2.0)) ==== metre(4.0)        
      }
      "Cancel DNils" in {
        def mult[L <: Dims](l: Scalar[L], r: Scalar[DNil]): Scalar[L] = l * r
        mult(second(2.0),coef(2.0)) ==== second(4.0)

        def mult2[R <: Dims](l: Scalar[DNil], r: Scalar[R]): Scalar[R] = l * r
        mult2(coef(2.0),second(2.0)) ==== second(4.0)
      }
    }
    "Divide" in {
      "Cancel self" in {
        def div[L <: Dims](l: Scalar[L], r: Scalar[L]): Scalar[DNil] = l / r
        div(metre(4.0),metre(2.0)) ==== coef(2.0)
      }
      "Cancel numerators" in {
        def div[L <: Dims, R <: Dims](l: Scalar[L], r: Scalar[L#Div[R]]): Scalar[R] = l / r
        div[Length,Time](metre(2.0), metrePerSecond(2.0)) ==== second(1.0)

        def div2[L <: Dims, R <: Dims](l: Scalar[R#Div[L]], r: Scalar[R]): Scalar[L#Neg] = l / r
        div2[Time,Length](metrePerSecond(4.0), metre(2.0)) ==== hertz(2.0)
      }
      "Cancel DNils" in {
        def div[L <: Dims](l: Scalar[L], r: Scalar[DNil]): Scalar[L] = l / r
        div(second(2.0),coef(2.0)) ==== second(1.0)

        def div2[R <: Dims](l: Scalar[DNil], r: Scalar[R]): Scalar[R#Neg] = l / r
        div2(coef(2.0),second(2.0)) ==== hertz(1.0)
      }
    }
  }*/
}