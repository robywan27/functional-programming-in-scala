package exercises.chapter6.state

import chapters.chapter6_state.State
import chapters.chapter6_state.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State((m: Machine) => {
      if (m.candies > 0) {
        inputs match {
          case Nil => ((m.coins, m.candies), m)
          case i :: is => i match {
            case Coin =>
              if (m.locked) simulateMachine(is).run(modify((m: Machine) => Machine(false, m.candies, m.coins + 1)).run(m)._2)
              else simulateMachine(is).run(m)
            case Turn =>
              if (!m.locked) simulateMachine(is).run(modify((m: Machine) => Machine(true, m.candies - 1, m.coins)).run(m)._2)
              else simulateMachine(is).run(m)
          }
        }
      }
      else ((m.coins, m.candies), m)
    })
  // Repo solution below


  def main(args: Array[String]): Unit = {
    println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10)))   // ((14,1),Machine(true,1,14))
    println(simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(true, 10, 10)))   // ((12,8),Machine(true,8,12))
    println(simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(false, 10, 10)))   // ((11,8),Machine(true,8,11))
    println(simulateMachine(List(Coin, Coin, Turn, Turn)).run(Machine(true, 10, 10)))   // ((11,9),Machine(true,9,11))
    println(simulateMachine(List(Turn, Coin, Coin, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 10, 10)))   // ((14,6),Machine(true,6,14))
    println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 3, 10)))   // ((13,0),Machine(true,0,13))
  }


  // Repo solution
   /*def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)*/
}