package uk.kamchatka.fpis

import uk.kamchatka.fpis.Monoid.compositionMonoid
import uk.kamchatka.fpis.Stream.{Cons, Empty, cons, empty}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a, bs) => cons(f(a), bs))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, acc) => if (f(a)) cons(a, acc) else acc)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)(f(_).append(_))

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }
}

object Stream {

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def foldMap[A, B](as: Stream[A], m: Monoid[B])(f: A => B): B =
    Stream.foldLeft(as, m.zero)((b, a) => m.op(b, f(a)))

  def foldLeft2[A, B](as: Stream[A], z: B)(f: (B, A) => B): B =
    foldMap(as, compositionMonoid[B])(a => (b: B) => f(b, a))(z)

  def foldLeft[A, B](as: Stream[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def fold(acc: B, xs: Stream[A]): B = xs match {
      case Empty => acc
      case Cons(h, rest) => fold(f(acc, h()), rest())
    }

    fold(z, as)
  }

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)

  def length[A](as: Stream[A]): Int = foldLeft(as, 1)((a, _) => a + 1)

  def reverse[A](as: Stream[A]): Stream[A] = foldLeft[A, Stream[A]](as, Empty)((as, a) => cons(a, as))

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = (as, bs) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(), h2()), zipWith(t1(), t2())(f))
  }

  @tailrec
  def startsWith[A](a: Stream[A], b: Stream[A]): Boolean = (a, b) match {
    case (_, Empty) => true
    case (Cons(ah, at), Cons(bh, bt)) => ah() == bh() && startsWith(at(), bt())
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](a: Stream[A], b: Stream[A]): Boolean =
    startsWith(a, b) || (a != Empty && hasSubsequence(tail(a), b))

  def tail[A](xs: Stream[A]): Stream[A] = xs match {
    case Empty => sys.error("tail of an empty Stream")
    case Cons(_, t) => t()
  }

  def init[A](xs: Stream[A]): Stream[A] = xs match {
    case Empty => sys.error("init of an empty Stream")
    case Cons(h, t) =>
      if (t() == Empty) Empty
      else cons(h(), init(t()))
  }

  @tailrec
  def drop[A](n: Int, xs: Stream[A]): Stream[A] =
    if (n == 0) xs
    else xs match {
      case Empty => sys.error(s"drop($n) from an empty Stream")
      case Cons(_, t) => drop(n - 1, t())
    }

  @tailrec
  def dropWhile[A](xs: Stream[A])(p: A => Boolean): Stream[A] =
    xs match {
      case Empty => Empty
      case Cons(h, t) if p(h()) => dropWhile(t())(p)
      case _ => xs
    }

  def setHead[A](a: => A)(xs: Stream[A]): Stream[A] = xs match {
    case Empty => sys.error("tail of an empty Stream")
    case Cons(_, t) => cons(a, t())
  }
}
