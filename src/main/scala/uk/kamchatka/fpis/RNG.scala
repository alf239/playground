package uk.kamchatka.fpis

import uk.kamchatka.fpis.RNG._

trait RNG {
  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt
  val double: Rand[Double] = map(int)(toDouble)

  def nonNegativeInt: (Int, RNG) =
    map(int)(nonNegative)(this)

  def nextDouble: (Double, RNG) =
    double(this)

  def intDouble: ((Int, Double), RNG) =
    both(int, double)(this)

  def doubleInt: ((Double, Int), RNG) =
    both(double, int)(this)

  def double3: ((Double, Double, Double), RNG) =
    map(both(double, both(double, double))) {
      case (d0, (d1, d2)) => (d0, d1, d2)
    }(this)

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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng0) = ra(rng)
      val (b, rng1) = rb(rng0)
      (f(a, b), rng1)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def nonNegative(n: Int): Int = if (n >= 0) n else ~n

  private val Range: Double = Int.MaxValue.toDouble + 1.0

  def toDouble(n: Int): Double = nonNegative(n) / Range

}