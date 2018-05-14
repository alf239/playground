package uk.kamchatka.fpis

import uk.kamchatka.fpis.parallelism.Par.Par

import scala.annotation.tailrec
import scala.language.{higherKinds, reflectiveCalls}

trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] =
    flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)
}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]


  def unit[F[_], A](a: A): Free[F, A] = Return(a)

  @tailrec
  def runTrampoline[A](tr: TailRec[A]): A = tr match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y flatMap (a => g(a) flatMap f))
    }
  }

  @tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)
                            (implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(x, f) => x match {
        case Suspend(r) => G.flatMap(t(r))(a => runFree(f(a))(t))
        case _ => sys.error("Impossible; `step` eliminates these cases")
      }
    }

  implicit def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      override def unit[A](a: A): Free[F, A] = Free.unit(a)

      def compose[A, B, C](f: A => Free[F, B],
                           g: B => Free[F, C]): A => Free[F, C] = a => f(a) flatMap g
    }
}
