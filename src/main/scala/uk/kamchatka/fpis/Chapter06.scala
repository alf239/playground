package uk.kamchatka.fpis

object Chapter06 {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    import State._

    def action(input: Input): State[Machine, Unit] =
      modify(m =>
        input match {
          case Coin if m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
          case Turn if m.candies > 0 && !m.locked => m.copy(locked = true, candies = m.candies - 1)
          case _ => m
        })

    for {
      _ <- sequence(inputs map action)
      m <- get
    } yield (m.coins, m.candies)
  }

}
