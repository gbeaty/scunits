package scunits.test

import scunits._
import Scunits._
import scunits.quantity._
import scunits.us._
import scunits.typeclass.TCMeasure
import scunits.typeclass.DListOps._

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

  def ×(r: Double) = ValueClass(v / r)
  def ÷(r: Double) = ValueClass(v * r)
}
case class ImpValueClass[D <: Dims](v: Double) extends AnyVal {
  def +[R <: Dims](r: ImpValueClass[R])(implicit m: Add[D,R]) = ImpValueClass[D](v + r.v)
  def -[R <: Dims](r: ImpValueClass[R])(implicit m: Sub[D,R]) = ImpValueClass[D](v - r.v)
  def /[R <: Dims](r: ImpValueClass[R])(implicit m: Mult[D,R]) = ImpValueClass[m.Out](v / r.v)
  def *[R <: Dims](r: ImpValueClass[R])(implicit m: Div[D,R]) = ImpValueClass[m.Out](v * r.v)

  def /(r: Double) = ImpValueClass[D](v / r)
  def *(r: Double) = ImpValueClass[D](v * r)
}
trait Add[L <: Dims, R <: Dims]
trait Sub[L <: Dims, R <: Dims]
trait Mult[L <: Dims, R <: Dims] {
  type Out <: Dims
}
trait Multed[L <: Dims, R <: Dims, E <: Dims] extends Mult[L,R] {
  type Out = E
}
trait Div[L <: Dims, R <: Dims] {
  type Out <: Dims
}
trait Dived[L <: Dims, R <: Dims, E <: Dims] extends Div[L,R] {
  type Out = E
}

case class BenchResult[A](name: String, result: A, time: Long, cycles: Long)

class Benchmarks extends Specification {
  sequential

  implicit def add[L <: Dims, R <: Dims]: Add[L,R] = null
  implicit def sub[L <: Dims, R <: Dims]: Sub[L,R] = null
  implicit def mult[L <: Dims, R <: Dims]: Multed[L,R,L#Mult[R]] = null
  implicit def div[L <: Dims, R <: Dims]:  Dived[L,R,L#Div[R]] = null

  def time[A](f: () => A)(name: String, cycles: Long) = {
    val start = System.nanoTime
    val res = f()
    val time = (System.nanoTime - start)
    BenchResult(name, res, time, cycles)
  }

  def BenchArithmetic(cycles: Int) = {
    val its = 1000000
    val vals = Array.fill(its)(util.Random.nextDouble)

    val dRes = time(() => {
      var i = 0
      var res = 1.0
      while(i < its) {
        val el = vals(i)
        res = res + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el
        i += 1
      }
      res
    })("Double",cycles)

    val mRes = time(() => {
      var i = 0
      var res = Measure[Acceleration](1.0)
      while(i < its) {
        val el = Measure[Acceleration](vals(i))
        res = res + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el
        i += 1
      }
      res.v
    })("Measure",cycles)

    val bRes = time(() => {
      var i = 0
      var res = Boxed(1.0)
      while(i < its) {
        val el = Boxed(vals(i))
        res = res + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el
        i += 1
      }
      res.v
    })("Boxed",cycles)

    val iRes = time(() => {
      var i = 0
      var res = ImpValueClass[Acceleration](1.0)
      while(i < its) {
        val el = ImpValueClass[Acceleration](vals(i))
        res = res + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el + res * (res / 2.0) * 1.5 / res - el * (el / 2.0) * 1.5 / el
        i += 1
      }
      res.v
    })("ImpValueClass",cycles)

    val vRes = time(() => {
      var i = 0
      var res = ValueClass(1.0)
      while(i < its) {
        val el = ValueClass(vals(i))
        res = res + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el
        i += 1
      }
      res.v
    })("ValueClass",cycles)

    val tRes = time(() => {
      var i = 0
      var res = TCMeasure[typeclass.Length](1.0)
      while(i < its) {
        val el = TCMeasure[typeclass.Length](vals(i))
        res = res + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el + res * (res ÷ 2.0) × 1.5 / res - el * (el ÷ 2.0) × 1.5 / el
        i += 1
      }
      res.v
    })("TCMeasure",cycles)

    Seq(dRes, mRes, bRes, vRes, iRes, tRes)
  }

  def BenchArray(arraySize: Int) = {
    val double = Vector.fill(arraySize)(util.Random.nextDouble).toArray
    val measureA = ArrayM[Length](double)
    val measure = double.map(Measure[Length](_))

    import scala.collection.mutable._

    def iterate[A](a: ArrayLike[A,_], sum: (A,A) => A) = {
      var i = 0;
      while(i < a.length) {

        var ii = i
        while(ii < a.length) {
          a(i) = sum(a(i), a(ii))
          ii += 1
        }

        i += 1
      }
    }

    val mop = (l: Measure[Length], r: Measure[Length]) => l + r

    val cycles = arraySize * arraySize / 2

    Seq(
      time(() => iterate(double, (l: Double, r: Double) => l + r))("Array[Double]",cycles),
      time(() => iterate(measureA, mop))("ArrayM[Measure[Length]]",cycles),
      time(() => iterate(measure, mop))("Array[Measure[Length]]",cycles)
    )
  }

  val preJitRes = BenchArithmetic(10000)
  val jitRes = BenchArithmetic(500000)
  val preJitArray = BenchArray(300)
  val jitArray = BenchArray(4000)

  def printAll(brs: (String,Seq[BenchResult[_]])*) {    
    brs.foreach { brn =>
      val (n,br) = brn
      println()
      println("-- " + n)
      br.foreach { r => println(r.name + ": " + r.time / r.cycles + " ns/cycle") }
    }
  }

  printAll(
    "Pre-JIT Arithmetic"->preJitRes,
    "JITed Arithmetic"->jitRes,
    "Pre-JIT Array"->preJitArray,
    "JITed Array"->jitArray
  )
  println()

  "All tests" should {
    "Produce the same results as Doubles" in {
      Seq(preJitRes, jitRes).map { brs =>
        brs.map(_.result).toSet.size ==== 1
      }
    }
  }

  "Measures" should {
    "Out-perform boxed values before JIT has warmed up" in {
      preJitRes(1).time must be_<(preJitRes(2).time)
    }
    "Perform similarly to Doubles once it has warmed up" in {
      jitRes(1).time must be_<((jitRes(0).time * 1.1).toLong)
    }
  }

  "ArrayM[_]s" should {
    "Out-perform Array[Measure[_]]" in {
      jitArray(1).time must be_<(jitArray(2).time)
    }
    "Perform like Array[Double]" in {
      jitArray(1).time must be_<((jitArray(0).time * 1.1).toLong)
    }
  }
}