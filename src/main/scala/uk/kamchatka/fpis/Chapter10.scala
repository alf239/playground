package uk.kamchatka.fpis

import uk.kamchatka.fpis.collections.List.foldLeft2

object Chapter10 extends App {

  println(foldLeft2(collections.List(1 to 10: _*), "see")((b, a) => b + "," + a))
}
