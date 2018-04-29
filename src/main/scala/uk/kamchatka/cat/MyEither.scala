package uk.kamchatka.cat

sealed trait MyEither[T]

case class MyLeft[T](v: T) extends MyEither[T]

case class MyRight[T](v: T) extends MyEither[T]

