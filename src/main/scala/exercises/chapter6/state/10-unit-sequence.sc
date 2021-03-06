import chapters.chapter6_state.State

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  /*State(s => {
    val (a, sa) = this.run(s)
    (f(a), sa)
  })*/

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a =>
      sb.map(b =>
        f(a, b)
      ))
  /*State(s => {
    val (a, sa) = this.run(s)
    val (b, sb2) = sb.run(sa)
    (f(a, b), sb2)
  })*/

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, sa) = this.run(s)
      g(a).run(sa)
    })
}

def unit[S, A](a: A): State[S, A] =
  State(s => (a, s))

def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
  fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

