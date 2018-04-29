package uk.kamchatka.cat

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

class Chapter01Test extends FunSpec with PropertyChecks {
  def compose[A, B, C]: (A => B) => (B => C) => A => C = Chapter01.compose[A, B, C]

  def id[A]: A => A = Chapter01.identity[A]

  describe("compose") {
    it("should respect identity") {
      forAll { (a: Long, f: Long => Double) =>
        assert(compose(id[Long])(f)(a) === f(a))
        assert(compose(f)(id)(a) === f(a))
      }
    }
  }
}
