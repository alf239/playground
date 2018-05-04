package uk.kamchatka.fpis.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import uk.kamchatka.fpis.Monoid

import scala.concurrent.{ExecutionContext, Promise}

object Par {

  sealed trait Future[A] {
    private[parallelism] def apply(cb: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => new Future[A] {
    override private[parallelism] def apply(cb: A => Unit): Unit = cb(a)
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      override private[parallelism] def apply(cb: C => Unit): Unit = {
        val ap = Promise[A]()
        val bp = Promise[B]()

        pa(es)(a => ap.success(a))
        pb(es)(b => bp.success(b))

        implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)
        for (a <- ap.future; b <- bp.future) cb(f(a, b))
      }
    }

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      override private[parallelism] def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    as.foldRight(unit(List.empty[A])) {
      (a, acc) =>
        map2(a, acc)(_ :: _)
    }

  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    val filters = as.map(asyncF(Some(_).filter(p)))
    val filtered = sequence(filters)
    map(filtered)(_.flatten)
  }

  def parFoldMap[A, B: Monoid](as: IndexedSeq[A])(f: A => B): Par[B] = {
    val m = implicitly[Monoid[B]]
    if (as.length <= 1)
      Par.unit(as.headOption map f getOrElse m.zero)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(parFoldMap(l)(f)), Par.fork(parFoldMap(r)(f)))(m.op)
    }
  }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }
    latch.await()
    ref.get()
  }
}