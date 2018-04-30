package uk.kamchatka.fpis

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import uk.kamchatka.fpis.collections.Either.{sequence, traverse}

class EitherTest extends FunSpec with PropertyChecks {
  describe("Left") {
    val left = collections.Either.left[Throwable, Long](new Throwable("Test exception"))
    it("maps to Left") {
      forAll { f: (Long => Double) =>
        assert(left.map(f) === left)
      }
    }
    it("flatMaps to Left") {
      forAll { f: (Long => Double) =>
        assert(left.flatMap(x => collections.Either.right(f(x))) === left)
      }
    }
    it("defaults to the parameter") {
      forAll { d: Long =>
        assert(left.orElse(collections.Right(d)) === collections.Right(d))
        val otherLeft = collections.Left(new RuntimeException("Another exception"))
        assert(left.orElse(otherLeft) === otherLeft)
      }
    }
  }
  describe("Right") {
    it("maps to Right") {
      forAll { (x: Long, f: Long => Double) =>
        assert(collections.Right(x).map(f) === collections.Right(f(x)))
      }
    }
    it("flatMaps to Right") {
      forAll { (x: Long, f: Long => Double) =>
        assert(collections.Right(x).flatMap(x => collections.Right(f(x))) === collections.Right(f(x)))
      }
    }
    it("defaults to itself in orElse") {
      forAll { (x: Long, d: Long) =>
        assert(collections.Right(x).orElse(collections.Right(d)) === collections.Right(x))
        assert(collections.Right(x).orElse(collections.Left(new RuntimeException("Look, exception!"))) === collections.Right(x))
      }
    }
  }
  describe("sequence") {
    it("works for a simple example") {
      assert(sequence(collections.List(collections.Right(1), collections.Right(2), collections.Right(3))) === collections.Right(collections.List(1, 2, 3)))
    }
    it("works for a simple example with Left") {
      val left = collections.Left(new Exception("test"))
      assert(sequence(collections.List(collections.Right(1), left, collections.Right(3))) === left)
    }
  }
  describe("traverse") {
    it("works for a simple example") {
      assert(traverse(collections.List(1, 2, 3))(x => collections.Right(x * x)) === collections.Right(collections.List(1, 4, 9)))
    }
    it("works for a simple example with Left") {
      val left = collections.Left(new Exception("test again"))
      assert(traverse(collections.List(1, 2, 3))(x => if (x % 2 == 0) left else collections.Right(x * x)) === left)
    }
  }
}
