package uk.kamchatka.fpis

import scala.annotation.tailrec


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum1[A](as: List[A])(implicit ev: Numeric[A]): A = foldLeft(as, ev.zero)(ev.plus)

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product1[A](as: List[A])(implicit ev: Numeric[A]): A = foldLeft(as, ev.one)(ev.times)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def fold(acc: B, xs: List[A]): B = xs match {
      case Nil => acc
      case Cons(h, rest) => fold(f(acc, h), rest)
    }

    fold(z, as)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)(flip(f))

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)

  def length[A](as: List[A]): Int = foldLeft(as, 1)((a, _) => a + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft[A, List[A]](as, Nil)((as, a) => Cons(a, as))

  def append[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)(Cons(_, _))

  def flatten[A](ass: List[List[A]]): List[A] = foldRight[List[A], List[A]](ass, Nil)(append)

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight[A, List[B]](as, Nil)((a, bs) => Cons(f(a), bs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil)((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight[A, List[B]](as, Nil)((a, bs) => append(f(a), bs))

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @tailrec
  def startsWith[A](a: List[A], b: List[A]): Boolean = (a, b) match {
    case (_, Nil) => true
    case (Cons(ah, at), Cons(bh, bt)) => ah == bh && startsWith(at, bt)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean =
    startsWith(a, b) || (a != Nil && hasSubsequence(tail(a), b))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("tail of an empty List")
    case Cons(_, t) => t
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("init of an empty List")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  @tailrec
  def drop[A](n: Int, xs: List[A]): List[A] =
    if (n == 0) xs
    else xs match {
      case Nil => sys.error(s"drop($n) from an empty List")
      case Cons(_, t) => drop(n - 1, t)
    }

  @tailrec
  def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] =
    xs match {
      case Nil => Nil
      case Cons(h, t) if p(h) => dropWhile(t)(p)
      case _ => xs
    }

  def setHead[A](a: A)(xs: List[A]): List[A] = xs match {
    case Nil => sys.error("tail of an empty List")
    case Cons(_, t) => Cons(a, t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
