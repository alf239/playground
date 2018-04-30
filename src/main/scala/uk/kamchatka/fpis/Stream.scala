package uk.kamchatka.fpis

import uk.kamchatka.fpis.Monoid.compositionMonoid

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def toList: List[A]

  def take(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil

  override def take(n: Int): Stream[Nothing] = Empty

  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = h() :: t().toList

  override def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else Cons(h, () => t().take(n - 1))

  override def takeWhile(p: A => Boolean): Stream[A] = {
    lazy val head = h()
    if (p(head)) Cons(() => head, () => t().takeWhile(p))
    else Empty
  }
}

object Stream {
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

  def append[A](a: Stream[A], b: => Stream[A]): Stream[A] = a.foldRight(b)(cons(_, _))

  def flatten[A](ass: Stream[Stream[A]]): Stream[A] = ass.foldRight[Stream[A]](empty)(append)

  def map[A, B](as: Stream[A])(f: A => B): Stream[B] = as.foldRight[Stream[B]](empty)((a, bs) => cons(f(a), bs))

  def filter[A](as: Stream[A])(f: A => Boolean): Stream[A] =
    as.foldRight[Stream[A]](empty)((a, acc) => if (f(a)) cons(a, acc) else acc)

  def filter2[A](as: Stream[A])(f: A => Boolean): Stream[A] =
    flatMap(as)(a => if (f(a)) Stream(a) else Empty)

  def flatMap[A, B](as: Stream[A])(f: A => Stream[B]): Stream[B] =
    as.foldRight[Stream[B]](Empty)((a, bs) => append(f(a), bs))

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
