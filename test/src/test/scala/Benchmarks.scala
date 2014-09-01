package scunits.test

import scunits._
import scunits.quantity._
import scunits.unit.acceleration.gee

import org.specs2.mutable._

class Benchmarks extends Specification {

  case class Boxed(v: Double) {
    def +(r: Boxed) = Boxed(v + r.v)
    def /(r: Double) = Boxed(v / r)
  }

  val num = 5000000
  val doubles = Vector.fill(num)(util.Random.nextDouble)
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

  val (dSum, dTime) = time(() => doubles.foldLeft(0.0)(_+_))
  val (mSum, mTime) = time(() => measures.foldLeft(Measure[Acceleration](0.0))(_+_))
  val (bSum, bTime) = time(() => boxes.foldLeft(Boxed(0.0))(_+_))

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

  "Measures" should {
    "Out-perform boxed values" in {
      mTime must be_<(bTime)
    }
  }
}