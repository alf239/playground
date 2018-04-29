package uk.kamchatka.fpis

import scala.language.higherKinds

trait Monad[F[_]] {
  def unit[A](a: A): F[A]

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]

  def flatMap[A, B](x: F[A])(f: A => F[B]): F[B] =
    compose[Unit, A, B](_ => x, f)(())
}

