package uk.kamchatka.fpis

trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: Option[B]): Option[B]

  def filter(p: A => Boolean): Option[A]
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: Option[B]): Option[B] = ob

  override def filter(p: Nothing => Boolean): Option[Nothing] = None
}

case class Some[+A](a: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(a))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(a)

  override def getOrElse[B >: A](default: => B): B = a

  override def orElse[B >: A](ob: Option[B]): Option[B] = this

  override def filter(p: A => Boolean): Option[A] = if (p(a)) this else None
}

object Option {
  def none[A]: Option[A] = None

  def lift[A, B](a: Option[A], b: Option[B])(f: A => B): Option[B] =
    for (x <- a) yield f(x)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for (x <- a; y <- b) yield f(x, y)

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    for (x <- a; y <- b; z <- c) yield f(x, y, z)

  def sequence[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(identity)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case Cons(x, xs) => for (ot <- traverse(xs)(f); oh <- f(x)) yield Cons(oh, ot)
  }
}