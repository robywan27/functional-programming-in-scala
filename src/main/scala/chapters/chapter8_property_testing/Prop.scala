package chapters.chapter8_property_testing

import chapters.chapter5_laziness.Stream
import chapters.chapter6_state.RNG
import chapters.chapter8_property_testing.Prop._
import chapters.chapter8_property_testing.Gen._
import chapters.chapter8_property_testing.SGen._

case class Prop(run: (MAxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop =
    ???

  def ||(p: Prop): Prop = ???
}


object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MAxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
//  case object Proved extends Result
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = ???

  /*def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }*/

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  /*def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)*/

  /*def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take(n.min(max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {
        (max, _, rng) => p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }*/

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      /*case Proved =>
        println(s"+ OK, proved property.")*/
    }


  /*def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf(smallInt)) {ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
  }*/
}
