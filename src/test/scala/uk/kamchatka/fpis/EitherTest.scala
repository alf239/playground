package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import uk.kamchatka.fpis.Either.{sequence, traverse}

class EitherTest extends FunSpec with PropertyChecks {
  describe("Left") {
    val left = Either.left[Throwable, Long](new Throwable("Test exception"))
    it("maps to Left") {
      forAll { f: (Long => Double) =>
        assert(left.map(f) === left)
      }
    }
    it("flatMaps to Left") {
      forAll { f: (Long => Double) =>
        assert(left.flatMap(x => Either.right(f(x))) === left)
      }
    }
    it("defaults to the parameter") {
      forAll { d: Long =>
        assert(left.orElse(Right(d)) === Right(d))
        val otherLeft = Left(new RuntimeException("Another exception"))
        assert(left.orElse(otherLeft) === otherLeft)
      }
    }
  }
  describe("Right") {
    it("maps to Right") {
      forAll { (x: Long, f: Long => Double) =>
        assert(Right(x).map(f) === Right(f(x)))
      }
    }
    it("flatMaps to Right") {
      forAll { (x: Long, f: Long => Double) =>
        assert(Right(x).flatMap(x => Right(f(x))) === Right(f(x)))
      }
    }
    it("defaults to itself in orElse") {
      forAll { (x: Long, d: Long) =>
        assert(Right(x).orElse(Right(d)) === Right(x))
        assert(Right(x).orElse(Left(new RuntimeException("Look, exception!"))) === Right(x))
      }
    }
  }
  describe("sequence") {
    it("works for a simple example") {
      assert(sequence(List(Right(1), Right(2), Right(3))) === Right(List(1, 2, 3)))
    }
    it("works for a simple example with Left") {
      val left = Left(new Exception("test"))
      assert(sequence(List(Right(1), left, Right(3))) === left)
    }
  }
  describe("traverse") {
    it("works for a simple example") {
      assert(traverse(List(1, 2, 3))(x => Right(x * x)) === Right(List(1, 4, 9)))
    }
    it("works for a simple example with Left") {
      val left = Left(new Exception("test again"))
      assert(traverse(List(1, 2, 3))(x => if (x % 2 == 0) left else Right(x * x)) === left)
    }
  }
}
