package uk.kamchatka.fpis

import uk.kamchatka.fpis.collections.{None, Some}

object Chapter04 {
  def mean(xs: Seq[Double]): collections.Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): collections.Option[Double] =
    for {
      mu <- mean(xs)
      squares = xs map (x => math.pow(x - mu, 2))
      variance <- mean(squares)
    } yield variance

}
