package uk.kamchatka.fpis

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory, TimeUnit}

import uk.kamchatka.fpis.Monoid._
import uk.kamchatka.fpis.Par.{Par, parFoldMap}

object Chapter07 extends App {
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    parFoldMap(ints)(identity)(monoidSumInt)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    parFoldMap(ints)(identity)(monoidMaxInt)


  private val threadFactory: ThreadFactory = (r: Runnable) => {
    val t = new Thread(r)
    t.setDaemon(true)
    t
  }
  private val es: ExecutorService = Executors.newFixedThreadPool(1000, threadFactory)

  println(sum(1 to 100)(es).get(125, TimeUnit.MILLISECONDS))
  println(max(1 to 100)(es).get(125, TimeUnit.MILLISECONDS))
  println(
    Par.sequence {
      (1 to 30).toList map Par.asyncF((x: Int) => x * x)
    }(es).get(125, TimeUnit.MILLISECONDS))
}
