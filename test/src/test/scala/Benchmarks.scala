package scunits.test

import scunits._
import scunits.quantity._
import scunits.us._

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

case class BenchResult[A](name: String, result: A, time: Long)
case class BenchResults(name: String, results: Seq[BenchResult[_]])

class Benchmarks extends Specification {
  sequential

  def time[A](f: () => A)(name: String) = {
      val start = System.nanoTime
      val res = f()
      val time = (System.nanoTime - start)
      BenchResult(name, res, time)
    }

  def BenchArithmetic(cycles: Int) = {
    val doubles = Vector.fill(cycles)(util.Random.nextDouble)
    val measures = doubles.map(Measure[Acceleration](_))
    val boxes = doubles.map(Boxed(_))
    val valueClasses = doubles.map(ValueClass(_))    

    doubles.foldLeft(0.0)(_+_)
    measures.foldLeft(Measure[Acceleration](0.0))(_+_)
    boxes.foldLeft(Boxed(0.0))(_+_)
    valueClasses.foldLeft(ValueClass(0.0))(_+_)

    val dRes = time(() => doubles.foldLeft(0.0) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })("Double")
    val mRes = time(() => measures.foldLeft(Measure[Acceleration](0.0)) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    }.v)("Measure")
    val bRes = time(() => boxes.foldLeft(Boxed(0.0)) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    }.v)("Boxed")
    val vRes = time(() => valueClasses.foldLeft(ValueClass(0.0)) { (res,el) =>
      res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5
    }.v)("ValueClass")

    Seq(dRes, mRes, bRes, vRes)
  }

  def BenchArray(cycles: Int) = {
    val double = Vector.fill(cycles)(util.Random.nextDouble).toArray
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

    Seq(
      time(() => iterate(double, (l: Double, r: Double) => l + r))("Array[Double]"),
      time(() => iterate(measureA, mop))("ArrayM[Measure[Length]]"),
      time(() => iterate(measure, mop))("Array[Measure[Length]]")
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
      br.foreach { r => println(r.name + ": " + r.time / 1000000 + " ms") }
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