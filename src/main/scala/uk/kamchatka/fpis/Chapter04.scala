package uk.kamchatka.fpis

object Chapter04 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    for {
      mu <- mean(xs)
      squares = xs map (x => math.pow(x - mu, 2))
      variance <- mean(squares)
    } yield variance

}
