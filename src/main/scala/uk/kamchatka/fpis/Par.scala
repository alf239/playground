package uk.kamchatka.fpis

sealed trait Par[+A]

object Par {

  case class Val[+A](a: A) extends Par[A]

  case class Fork[+A](a: () => Par[A]) extends Par[A]

  case class Apply2[-A, -B, +C](a: Par[A], b: Par[B], f: (A, B) => C) extends Par[C]

  def unit[A](a: A): Par[A] = Val(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = Apply2(a, b, f)

  def fork[A](a: => Par[A]): Par[A] = Fork(() => a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???
}