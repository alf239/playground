package uk.kamchatka.fpis

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](f: A => B)(g: (B, B) => B)(tree: Tree[A]): B = tree match {
    case Leaf(value) => f(value)
    case Branch(l, r) => g(fold(f)(g)(l), fold(f)(g)(r))
  }

  def size[A](t: Tree[A]): Int = fold[A, Int](_ => 1)(_ + _ + 1)(t)

  def maximum(t: Tree[Int]): Int = fold[Int, Int](identity)(_ max _)(t)

  def depth[A](t: Tree[A]): Int = fold[A, Int](_ => 1)(_ max _ + 1)(t)

  def map[A, B](f: A => B)(t: Tree[A]): Tree[B] = fold[A, Tree[B]](x => Leaf(f(x)))(Branch.apply)(t)
}