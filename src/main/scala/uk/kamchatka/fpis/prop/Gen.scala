package uk.kamchatka.fpis.prop

import uk.kamchatka.fpis.{RNG, State}

case class Gen[A](sample: State[RNG, A])

object Gen {
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def choose(min: Int, exclusiveMax: Int): Gen[Int] =
    Gen(RNG.nonNegativeLessThan(exclusiveMax - min) map (_ + min))

  def boolean: Gen[Boolean] = Gen(RNG.boolean)

  def listOf[A](g: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
}
