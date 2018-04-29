package uk.kamchatka.fpis

import scala.annotation.tailrec

object Chapter02 {
  def fibonacci(n: Int): Long = {
    @tailrec
    def fib(prev: Long, preprev: Long, n: Int): Long =
      if (n == 0) prev
      else fib(prev + preprev, prev, n - 1)

    if (n <= 0) 0
    else fib(1, 0, n - 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as.isEmpty || ((as, as.tail).zipped forall ordered)

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
}
