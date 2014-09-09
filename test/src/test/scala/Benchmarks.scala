package scunits.test

import scunits._
import scunits.quantity._

import org.specs2.mutable._

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
case class ValueClass(v: Double) extends AnyVal {
  def +(r: ValueClass) = ValueClass(v + r.v)
  def -(r: ValueClass) = ValueClass(v - r.v)
  def /(r: ValueClass) = ValueClass(v / r.v)
  def *(r: ValueClass) = ValueClass(v * r.v)

  def ÷(r: Double) = ValueClass(v / r)
  def ×(r: Double) = ValueClass(v * r)
}

class Benchmarks extends Specification {
  sequential  

  def BenchArithmetic(cycles: Int) = {
    val doubles = Vector.fill(cycles)(util.Random.nextDouble)
    val measures = doubles.map(Measure[Acceleration](_))
    val boxes = doubles.map(Boxed(_))
    val valueClasses = doubles.map(ValueClass(_))

    def time[A](f: () => A) = {
      val start = System.nanoTime
      val res = f()
      val time = (System.nanoTime - start)
      (res, time)
    }

    doubles.foldLeft(0.0)(_+_)
    measures.foldLeft(Measure[Acceleration](0.0))(_+_)
    boxes.foldLeft(Boxed(0.0))(_+_)
    valueClasses.foldLeft(ValueClass(0.0))(_+_)

    val (dSum, dTime) = time(() => doubles.foldLeft(0.0) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })
    val (mSum, mTime) = time(() => measures.foldLeft(Measure[Acceleration](0.0)) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })
    val (bSum, bTime) = time(() => boxes.foldLeft(Boxed(0.0)) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })
    val (vSum, vTime) = time(() => valueClasses.foldLeft(ValueClass(0.0)) { (res,el) =>
      res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5
    })

    def printTime(n: String, t: Long) = println(n + ": " + (t / 1000000) + " ms")

    println("Results, doubles, measures, boxes, valueClasses:")
    println(dSum)
    println(mSum)
    println(bSum)
    println(vSum)

    println()

    println("Times:")
    printTime("Doubles",dTime)
    printTime("Measures",mTime)
    printTime("Boxes",bTime)
    printTime("ValueClasses",vTime)

    (dTime, mTime, bTime, vTime)
  }

  "Measures" should {
    "Out-perform boxed values before JIT has warmed up" in {
      val (dTime, mTime, bTime, vTime) = BenchArithmetic(10000)
      mTime must be_<(bTime)
    }
    "Perform similarly to Doubles once it has warmed up" in {
      val (dTime, mTime, bTime, vTime) = BenchArithmetic(500000)
      mTime must be_<(dTime + dTime/10)
    }
  }
}