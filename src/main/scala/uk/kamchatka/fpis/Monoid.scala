package uk.kamchatka.fpis

trait Monoid[A] {
  def zero: A

  def op(a: A, b: A): A
}

object Monoid {

  val monoidSumInt: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a: Int, b: Int): Int = a + b
  }

  val monoidProdInt: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a: Int, b: Int): Int = a * b
  }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => m.zero

    override def op(a: A => B, b: A => B): A => B = x => m.op(a(x), b(x))
  }

  def compositionMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a: A => A, b: A => A): A => A = b compose a
  }
}
