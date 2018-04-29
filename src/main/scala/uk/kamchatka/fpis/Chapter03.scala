package uk.kamchatka.fpis

import scala.annotation.tailrec

object Chapter03 extends App {
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def fold(acc: B, xs: List[A]): B = xs match {
      case Nil => acc
      case h :: rest => fold(f(acc, h), rest)
    }

    fold(z, as)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)(flip(f))

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)

  def sum[A](as: List[A])(implicit ev: Numeric[A]): A = foldLeft(as, ev.zero)(ev.plus)

  def product[A](as: List[A])(implicit ev: Numeric[A]): A = foldLeft(as, ev.one)(ev.times)

  def length[A](as: List[A])(implicit ev: Numeric[A]): Int = foldLeft(as, 1)((a, _) => a + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List.empty[A])(_.::(_))

  def append[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)(_ :: _)

  def flatten[A](ass: List[List[A]]): List[A] = foldRight(ass, List.empty[A])(append)

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List.empty[B])(f(_) :: _)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List.empty[B])((a, bs) => append(f(a), bs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List.empty[A])((a, acc) => if (f(a)) a :: acc else acc)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case ((h1 :: t1), (h2 :: t2)) => f(h1, h2) :: zipWith(t1, t2)(f)
  }

  @tailrec
  def startsWith[A](a: List[A], b: List[A]): Boolean = (a, b) match {
    case (_, Nil) => true
    case (ah :: at, bh :: bt) => ah == bh && startsWith(at, bt)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean =
    startsWith(a, b) || (a.nonEmpty && hasSubsequence(a.tail, b))

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def fold[A, B](f: A => B)(g: (B, B) => B)(tree: Tree[A]): B = tree match {
    case Leaf(value) => f(value)
    case Branch(l, r) => g(fold(f)(g)(l), fold(f)(g)(r))
  }

  def size[A](t: Tree[A]): Int = fold[A, Int](_ => 1)(_ + _ + 1)(t)

  def maximum(t: Tree[Int]): Int = fold[Int, Int](identity)(_ max _)(t)

  def depth[A](t: Tree[A]): Int = fold[A, Int](_ => 1)(_ max _ + 1)(t)

  def map[A, B](f: A => B)(t: Tree[A]): Tree[B] = fold[A, Tree[B]](x => Leaf(f(x)))(Branch.apply)(t)

  println(foldLeft(Nil, 0)((_, _) => ???))
  println(sum(List(1, 2, 3)))
  println(length(List(1, 2, 3)))
  println(product(List(1, 2, 3)))
  println(reverse(List(1, 2, 3)))
  println(append(List(1, 2, 3), List(4, 5, 6)))
  println(flatten(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
  println(map(List(1, 2, 3))(_ + 1))
  println(map(List(1.0, 2.1, 3.14159))(_.toString))
  println(filter((1 to 10).toList)(_ % 2 == 0))
  println(filter2((1 to 10).toList)(_ % 2 == 0))
  println(flatMap((1 to 10).toList)(x => List(x, x + 0.5)))
  println(zipWith((1 to 10).toList, (1 to 10).map(_.toString).toList)((i, s) => s"int: $i, string: $s"))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4)))
  println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4, 6)))
  println(size(Leaf(1)))
  println(size(Branch(Leaf(1), Leaf(2))))
  println(maximum(Branch(Leaf(1), Leaf(2))))
  println(depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
  println(map((x: Int) => x * 2)(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
}
