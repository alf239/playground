package uk.kamchatka.fpis

import uk.kamchatka.fpis.RNG._

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG) = {
    val (n, rng) = nextInt
    (nonNegative(n), rng)
  }

  def nextDouble: (Double, RNG) = {
    val (n, rng) = nextInt
    (toDouble(n), rng)
  }

  def intDouble: ((Int, Double), RNG) = {
    val (n, rng0) = this.nextInt
    val (d, rng1) = rng0.nextDouble
    ((n, d), rng1)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val ((n, d), rng) = intDouble
    ((d, n), rng)
  }

  def double3: ((Double, Double, Double), RNG) = {
    val (d0, rng0) = this.nextDouble
    val (d1, rng1) = rng0.nextDouble
    val (d2, rng2) = rng1.nextDouble
    ((d0, d1, d2), rng2)
  }

  def ints(n: Int): (List[Int], RNG) =
    if (n <= 0) (Nil, this)
    else {
      val (m0, rng0) = this.nextInt
      val (ms, rng1) = rng0.ints(n - 1)
      (m0 :: ms, rng1)
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
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }


  def nonNegative(n: Int): Int = if (n >= 0) n else ~n

  private val Range: Double = Int.MaxValue.toDouble + 1.0

  def toDouble(n: Int): Double = nonNegative(n) / Range

}