package uk.kamchatka.ctfp

import scala.language.higherKinds

trait Functor[F[_]] {
  def fmap[A, B](f: A => B)(x: F[A]): F[B]
}

object Functor {
  def fmap[F[_], A, B](f: A => B)(x: F[A])(implicit ev: Functor[F]): F[B] = ev.fmap(f)(x)
}

trait Bifunctor[BF[_, _]] {
  def bimap[A, B, C, D](g: A => C, h: B => D)(x: BF[A, B]): BF[C, D] =
    second(h)(first(g)(x))

  def first[A, B, C](g: A => C)(x: BF[A, B]): BF[C, B] =
    bimap(g, identity[B])(x)

  def second[A, B, D](h: B => D)(x: BF[A, B]): BF[A, D] =
    bimap(identity[A], h)(x)
}

object Functors extends App {

  import Functor._

  type Reader[A, B] = A => B

  trait ReaderT[T] {
    type F[A] = Reader[T, A]
  }

  implicit def readerFunctor[T]: Functor[ReaderT[T]#F] = new Functor[ReaderT[T]#F] {
    override def fmap[A, B](f: A => B)(x: Reader[T, A]): Reader[T, B] = x andThen f
  }

  val result = fmap[ReaderT[String]#F, String, Int] {
    (s: String) => s.length
  } {
    (s: String) => log(s"Hello $s!")
  }

  println(result("Peter"))

  def log(s: String): String = {
    println(s)
    s
  }

}
