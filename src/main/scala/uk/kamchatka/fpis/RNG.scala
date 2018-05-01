package uk.kamchatka.fpis

import uk.kamchatka.fpis.RNG._
import uk.kamchatka.fpis.State.{both, sequence, unit}

trait RNG {
  def nextInt: (Int, RNG)

  val int: Rand[Int] = State(_.nextInt)
  val double: Rand[Double] = int.map(toDouble)

  def nonNegativeInt: (Int, RNG) =
    int.map(nonNegative).run(this)

  def nextDouble: (Double, RNG) =
    double.run(this)

  def intDouble: ((Int, Double), RNG) =
    both(int, double).run(this)

  def doubleInt: ((Double, Int), RNG) =
    both(double, int).run(this)

  def double3: ((Double, Double, Double), RNG) =
    both(double, both(double, double)).map {
      case (d0, (d1, d2)) => (d0, d1, d2)
    }.run(this)

  def ints(n: Int): (List[Int], RNG) =
    sequence(List.fill(n)(int)).run(this)

  def nonNegativeLessTnan(n: Int): Rand[Int] =
    int flatMap { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessTnan(n)
    }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) ^ 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[A] = State[RNG, A]

  def nonNegative(n: Int): Int = if (n >= 0) n else ~n

  private val Range: Double = Int.MaxValue.toDouble + 1.0

  def toDouble(n: Int): Double = nonNegative(n) / Range
}