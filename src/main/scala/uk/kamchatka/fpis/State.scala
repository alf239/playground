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

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    State(s => rs.foldRight((Nil: List[A], s)) {
      case (st, (as, s0)) =>
        val (a, s1) = st.run(s0)
        (a :: as, s1)
    })

  def both[S, A, B](sa: State[S, A], sb: State[S, B]): State[S, (A, B)] =
    sa.map2(sb)((_, _))
}