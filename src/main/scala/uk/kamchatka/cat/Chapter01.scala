package uk.kamchatka.cat

object Chapter01 {
  /** 1. Implement, as best as you can, the identity function in your favorite
    * language (or the second favorite, if your favorite language
    * happens to be Haskell). */
  def identity[A](a: A): A = a

  /** 2. Implement the composition function in your favorite language.
    * It takes two functions as arguments and returns a function that
    * is their composition. */
  def compose[A, B, C](f: A => B)(g: B => C): A => C = a => g(f(a))


}
