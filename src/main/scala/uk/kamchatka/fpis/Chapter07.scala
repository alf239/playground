package uk.kamchatka.fpis

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

import uk.kamchatka.fpis.Monoid._
import uk.kamchatka.fpis.parallelism.Par
import uk.kamchatka.fpis.parallelism.Par.{Par, parFoldMap}

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
  private val es: ExecutorService = Executors.newFixedThreadPool(10, threadFactory)

  println(Par.run(es)(sum(1 to 100)))
  println(Par.run(es)(max(1 to 100)))
  println(Par.run(es)(
    Par.sequence {
      (1 to 30).toList map Par.asyncF((x: Int) => x * x)
    }))
}
