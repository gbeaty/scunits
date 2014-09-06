package scunits.test

import scunits._
import scunits.quantity._

import org.specs2.mutable._

class Benchmarks extends Specification {
  sequential

  case class Boxed(v: Double) {
    def +(r: Boxed) = Boxed(v + r.v)
    def -(r: Boxed) = Boxed(v - r.v)
    def /(r: Boxed) = Boxed(v / r.v)
    def *(r: Boxed) = Boxed(v * r.v)

    def +(r: Double) = Boxed(v + r)
    def -(r: Double) = Boxed(v - r)
    def /(r: Double) = Boxed(v / r)
    def *(r: Double) = Boxed(v * r)
  }

  def BenchArithmetic(cycles: Int) = {
    val doubles = Vector.fill(cycles)(util.Random.nextDouble)
    val measures = doubles.map(Measure[Acceleration](_))
    val boxes = doubles.map(Boxed(_))

    def time[A](f: () => A) = {
      val start = System.nanoTime
      val res = f()
      val time = (System.nanoTime - start)
      (res, time)
    }

    doubles.foldLeft(0.0)(_+_)
    measures.foldLeft(Measure[Acceleration](0.0))(_+_)
    boxes.foldLeft(Boxed(0.0))(_+_)

    val (dSum, dTime) = time(() => doubles.foldLeft(0.0) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })
    val (mSum, mTime) = time(() => measures.foldLeft(Measure[Acceleration](0.0)) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })
    val (bSum, bTime) = time(() => boxes.foldLeft(Boxed(0.0)) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })

    def printTime(t: Long) = println((t / 1000000) + " ms")

    println("Results, doubles, measures, boxes:")
    println(dSum)
    println(mSum)
    println(bSum)

    println()

    println("Times, doubles, measures, boxes:")
    printTime(dTime)
    printTime(mTime)
    printTime(bTime)

    (dTime, mTime, bTime)
  }

  "Measures" should {
    val cycles = 500000
    "Out-perform boxed values before JIT has warmed up" in {
      val (dTime, mTime, bTime) = BenchArithmetic(cycles)
      mTime must be_<(bTime)
    }
    "Perform similarly to Doubles once it has warmed up" in {
      val (dTime, mTime, bTime) = BenchArithmetic(cycles)
      mTime must be_<(dTime + dTime/10)
    }
  }
}