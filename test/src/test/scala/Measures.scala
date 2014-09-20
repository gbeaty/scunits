package scunits.test

import scunits._
import scunits.default._
import scunits.si._

import org.specs2.mutable._

class Measures extends Specification {
  val m1 = metre(1.0)
  val m2 = metre(2.0)
  "Measures" should {    
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
    }
  }

  "Abstract measures" should {    
    "Add" in {
      def add[L <: Dims](l: Measure[L], r: Measure[L]): Measure[L] = l + r
      add(m1,m2) ==== metre(3.0)
    }
    "Subtract" in {
      def sub[L <: Dims](l: Measure[L], r: Measure[L]): Measure[L] = l - r
      sub(m1,m2) ==== metre(-1.0)
    }
    "Multiply" in {
      "Cancel denominators" in {
        def mult[L <: Dims, R <: Dims](l: Measure[L], r: Measure[R#Div[L]]): Measure[R] = l * r
        mult[Time,Length](second(2.0),metrePerSecond(2.0)) ==== metre(4.0)

        def mult2[L <: Dims, R <: Dims](l: Measure[L#Div[R]], r: Measure[R]): Measure[L] = l * r
        mult2[Length,Time](metrePerSecond(2.0),second(2.0)) ==== metre(4.0)        
      }
      "Cancel DNils" in {
        def mult[L <: Dims](l: Measure[L], r: Measure[DNil]): Measure[L] = l * r
        mult(second(2.0),coef(2.0)) ==== second(4.0)

        def mult2[R <: Dims](l: Measure[DNil], r: Measure[R]): Measure[R] = l * r
        mult2(coef(2.0),second(2.0)) ==== second(4.0)
      }
    }
    "Divide" in {
      "Cancel self" in {
        def div[L <: Dims](l: Measure[L], r: Measure[L]): Measure[DNil] = l / r
        div(metre(4.0),metre(2.0)) ==== coef(2.0)
      }
      "Cancel numerators" in {
        def div[L <: Dims, R <: Dims](l: Measure[L], r: Measure[L#Div[R]]): Measure[R] = l / r
        div[Length,Time](metre(2.0), metrePerSecond(2.0)) ==== second(1.0)

        def div2[L <: Dims, R <: Dims](l: Measure[R#Div[L]], r: Measure[R]): Measure[L#Neg] = l / r
        div2[Time,Length](metrePerSecond(4.0), metre(2.0)) ==== hertz(2.0)
      }
      "Cancel DNils" in {
        def div[L <: Dims](l: Measure[L], r: Measure[DNil]): Measure[L] = l / r
        div(second(2.0),coef(2.0)) ==== second(1.0)

        def div2[R <: Dims](l: Measure[DNil], r: Measure[R]): Measure[R#Neg] = l / r
        div2(coef(2.0),second(2.0)) ==== hertz(1.0)
      }
    }
  }
}