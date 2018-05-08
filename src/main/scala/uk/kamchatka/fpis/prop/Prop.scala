package uk.kamchatka.fpis.prop

import uk.kamchatka.fpis.prop.Prop.{Result, SuccessCount}
import uk.kamchatka.fpis.{RNG, Stream}

import scala.util.control.NonFatal


case class Prop(check: (SuccessCount, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (n, rng) => check(n, rng) && p.check(n, rng) }

  def ||(p: Prop): Prop = Prop { (n, rng) => check(n, rng) || p.check(n, rng) }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean

    def &&(b: => Result): Result = if (isFalsified) this else b

    def ||(b: => Result): Result = if (!isFalsified) this else b
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case NonFatal(e) => Falsified(buildMsgs(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsgs[A](a: A, e: Throwable): String =
    s"test case: $a\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n\t${e.getStackTrace.mkString("\n\t")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
}
