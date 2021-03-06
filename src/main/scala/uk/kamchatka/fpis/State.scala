package uk.kamchatka.fpis


case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight[State[S, List[A]]](unit(Nil)) {
      case (st, acc) => st.flatMap(a => acc.map(as => a :: as))
    }

  def both[S, A, B](sa: State[S, A], sb: State[S, B]): State[S, (A, B)] =
    sa.map2(sb)((_, _))
}