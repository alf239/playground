package uk.kamchatka.fpinscala

import scala.language.higherKinds

object Chapter10 extends App {

  trait Monoid[A] {
    def zero: A

    def op(a: A, b: A): A
  }

  trait Monad[F[_]] {
    def unit[A](a: A): F[A]

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]

    def flatMap[A, B](x: F[A])(f: A => F[B]): F[B] =
      compose[Unit, A, B](_ => x, f)(())
  }

  val monoidSumInt = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a: Int, b: Int): Int = a + b
  }

  val monoidProdInt = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a: Int, b: Int): Int = a * b
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val m: Monoid[B => B] = new Monoid[B => B] {
      override def zero: B => B = identity

      override def op(a: B => B, b: B => B): B => B = a andThen b
    }
    foldMap(as, m)(a => (b: B) => f(b, a))(z)
  }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => m.zero

    override def op(a: A => B, b: A => B): A => B = x => m.op(a(x), b(x))
  }

  println(foldLeft((1 to 10).toList, "see")((b, a) => b + "," + a))
}
