package uk.kamchatka.fpis

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get())

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  case class Map2Future[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

    override def isDone: Boolean = af.isDone && bf.isDone

    override def get(timeout: Long, unit: TimeUnit): C = {
      val t = System.nanoTime()
      val a = af.get(timeout, unit)
      val elapsed = System.nanoTime() - t
      val remaining = Math.max(unit.toNanos(timeout) - elapsed, 0)
      val b = bf.get(remaining, TimeUnit.NANOSECONDS)
      f(a, b)
    }

    override def get(): C = get(Long.MaxValue, TimeUnit.MILLISECONDS)
  }

}