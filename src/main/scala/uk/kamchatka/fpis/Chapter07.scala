package uk.kamchatka.fpis

import java.util.concurrent.{Executors, TimeUnit}

import uk.kamchatka.fpis.Par.Par

object Chapter07 extends App {
  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  println(sum(1 to 100)(Executors.newFixedThreadPool(1000)).get(125, TimeUnit.MILLISECONDS))
  println(
    Par.sequence {
      (1 to 30).toList map Par.asyncF((x: Int) => x * x)
    }(Executors.newFixedThreadPool(1000)).get(125, TimeUnit.MILLISECONDS))
}
