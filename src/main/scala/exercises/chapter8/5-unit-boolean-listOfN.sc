import chapters.chapter6_state.{RNG, State}
import chapters.chapter8_property_testing.Gen

def unit[A](a: => A): Gen[A] =
  Gen(State.unit(a))
//  Gen(State(rng => (a, rng))) - just use unit which is built from this implementation -> use higher-level abstractions

def boolean: Gen[Boolean] =
  Gen(State(RNG.boolean))

def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
  Gen(State.sequence(List.fill(n)(g.sample)))
//  Gen(State(rng => (List.fill(n)(g.sample.run(rng)._1), rng)))
