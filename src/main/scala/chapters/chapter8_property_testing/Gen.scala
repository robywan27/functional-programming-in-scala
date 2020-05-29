package chapters.chapter8_property_testing

import chapters.chapter6_state.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    ???

  def listOfN(size: Gen[Int]): Gen[List[A]] = // implementation with flatMap
    ??? //size.flatMap(i => Gen(State(List(i))))

  /*def listOfN(size: Int): Gen[List[A]] =
    listOfN(size, this)*/

  def unsized: SGen[A] =
    SGen(_ => this)

  def listOf: SGen[List[A]] = SGen.listOf(this)
}


object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = ???

}