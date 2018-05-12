package uk.kamchatka.fpis.io

import scala.annotation.tailrec


trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends IO[A]

case class Suspend[A](r: () => A) extends IO[A]

case class FlatMap[A, B](s: IO[A], k: A => IO[B]) extends IO[B]

object IO {
  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}