import chapters.chapter6_state.RNG

// quick workaround; worksheet cannot find these inside examples.chapter6_state.RNG
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val newInt = (newSeed >>> 16).toInt
    (newInt, nextRNG)
  }
}
type Rand[+A] = RNG => (A, RNG)
def unit[A](a: A): Rand[A] =
  rng => (a, rng)
val int: Rand[Int] = _.nextInt
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, ra2) = ra(rng)
    val (b, rb2) = rb(ra2)
    (f(a, b), rb2)
  }
//////////////////////////////////



def sequenceWithFold[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


// This solution is more concise than the onw below and uses foldRight.
// However it's kinda "cheating" because the function is defined as
// rng => foldRight rather than just in terms of foldRight
def sequenceFoldStyle[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng => fs.foldRight((List[A](), rng))((f, fs) => {
    val (v, newR) = f(fs._2)
    (v :: fs._1, newR)
  })


def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng => {
    @scala.annotation.tailrec
    def loop(fs: List[Rand[A]], r: RNG, vals: List[A]): (List[A], RNG) = {
      fs match {
        case Nil => (vals.reverse, r)
        case f :: fs =>
          val (v, newR) = f(r)
          loop(fs, newR, v :: vals)
      }
    }
    loop(fs, rng, List[A]())
  }



def ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))




val r: RNG = SimpleRNG(10)
sequence(List(unit(1), unit(2), unit(3)))(r)._1
sequenceFoldStyle(List(unit(1), unit(2), unit(3)))(r)._1
sequenceWithFold(List(unit(1), unit(2), unit(3)))(r)._1