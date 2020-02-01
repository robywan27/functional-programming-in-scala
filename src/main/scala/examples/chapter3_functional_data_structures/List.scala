package examples.chapter3_functional_data_structures

// abstract interface for defining data type List
  // + is the variance annotation, which means A is a covariant parameter of List
  // sealed means that all implementations of this trait must be declared in this file
sealed trait List[+A]
// List data constructor to represent the empty list
case object Nil extends List[Nothing]
// List data constructor to represent a non-empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]


// List companion object: contains functions for creating and working with lists
object List {
  // pattern matching on target data type; notice the recursive shape which mimics the recursive data structure quality
    // pattern => result
    // data type constructors are used as patterns
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  // pattern matching
  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  // this method acts as a List constructor: it takes some arguments and turns them into elements of a list
    // variadic function: it accepts zero or more arguments of type A
    // _* type annotation allows to pass a Seq to a variadic method
    // as is bound to Seq[A] which provides functions head and tail
    // Seq is the interface implemented by all sequence-like data structures in the Scala collections library
  def apply[A](as: A*): List[A] =
    if (as.isEmpty)   Nil
    else Cons(as.head, apply(as.tail: _*))


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("cannot take tail of empty list")
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], y: A): List[A] =
    l match {
      case Nil => sys.error("cannot modify head of empty list")
      case Cons(_, xs) => Cons(y, xs)
    }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0)   l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] =
    l match {
      // pattern guard: use if <cond> after the pattern
      case Cons(x, xs) if p(x) => dropWhile(xs, p)
      case _ => l
    }

  def append[A](l: List[A], m: List[A]): List[A] =
    l match {
      case Nil => m
      case Cons(x, xs) => Cons(x, append(xs, m))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("cannot take init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  /*
    Higher-order functions
   */
  def foldRight[A, B](as: List[A], v: B)(f: (A, B) => B): B =
    as match {
      case Nil => v
      case Cons(x, xs) => f(x, foldRight(xs, v)(f))
    }

  def sumR(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def productR(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)   // short-hand when types can be inferred and parameters are mentioned only once in the function body

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc)

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], v: B)(f: (B, A) => B): B =
    as match {
      case Nil => v
      case Cons(x, xs) => foldLeft(xs, f(v, x))(f)
    }

  def sumL(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)
  def productL(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)
  def lengthL[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  // Exercise  3.12
  /*def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, _) => Cons(_, acc))*/

  // Exercise  3.13

  def appendRL[A](l: List[A], m: List[A]): List[A] =
    foldRight(l, m)(Cons(_, _))

  // Exercise  3.15

  def increment(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, acc) => Cons(1 + x, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, acc) => if (p(x)) Cons(x, acc) else acc)

  // Exercise  3.20
  /*def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((x, acc) => Cons(f(x), acc))*/

  // Exercise  3.21

  def addLists(l: List[Int], m: List[Int]): List[Int] =
    (l, m) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
    }

  def zipWith[A](l: List[A], m: List[A])(f: (A, A) => A): List[A] =
    (l, m) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }


  def main(args: Array[String]): Unit = {
    // Exercise  3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x  // nope because after 2 you have 3
      case Nil => 42 // nope because it's not an empty list
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y   // match - result is 1 + 2 = 3
      case Cons(h, t) => h + sum(t)   // valid pattern, but the case before is matched so this one is not going to be the result
      case _ => 101   // this one is matched if none of the above are, but since the third case is matched, then this one is not the result
    }
    println(x)                    // 3 - the third pattern is matched so the result is 1 + 2

    val xs = List(6, 8, 9, 10, 7)
    val ys = List(1, 2, 3)
    // test Exercise 3.2
    println(tail(xs))             // List(8, 9, 10, 7)
    // println(tail(Nil))            // error: cannot take tail of empty list
    // test Exercise 3.3
    println(setHead(xs, 4))       // List(4, 8, 9, 10, 7)
    // note that xs is unchanged
    println(xs)                   // List(6, 8, 9, 10, 7)
    // test Exercise 3.4
    println(drop(xs, 3))          // List(10, 7)
    // test Exercise 3.5
    println(dropWhile(xs, (x: Int) => x % 2 == 0))  // List(9, 10, 7)
    // append
    println(append(xs, ys))       // List(6, 8, 9, 10, 7, 1, 2, 3)
    // test Exercise 3.6
    println(init(xs))             // List(6, 8, 9, 10)

    // Exercise 3.7
      // Recursion cannot be halted when 0.0 is found foldr goes all the way to the right until it reaches the base case,
      // i.e. when the list is empty. This is because of applicative evaluation of function parameters; f is not evaluated
      // as long as its arguments are evaluated

    // Exercise 3.8
    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))   // List(1, 2, 3)
    // foldr expresses the algorithm for creating the List data structure given the empty list and Cons function

    // test Exercise 3.9
    println(length(xs))           // 5
    // test Exercise 3.12
//    println(reverse(xs))          // List(7, 10, 9, 8, 6)
    // test Exercise 3.14
    println(appendRL(xs, ys))     // List(6, 8, 9, 10, 7, 1, 2, 3)
    // test Exercise 3.16
    println(increment(xs))        // List(7, 9, 10, 11, 8)
    // test Exercise 3.17
    println(doubleToString(List(1.2, 3.4, 5.6)))    // List("1.2", "3.4", "5.6")
    // test Exercise 3.18
    println(map(xs)((x) => x * x))       // List(36, 64, 81, 100, 49)
    println(map(xs)(_ * 2))              // List(12, 16, 18, 20, 14)
    // test Exercise 3.19
    println(filter(xs)(_ % 2 != 0))      // List(9, 7)

    // test Exercise 3.22
    println(addLists(xs, ys))           // List(7, 10, 12)
    println(addLists(xs, List(1, 2, 3, 6, 1)))      // List(7, 10, 12, 16, 8)
    println(addLists(xs, List(1, 2, 3, 6, 1, 4)))   // List(7, 10, 12, 16, 8)
    // test Exercise 3.23
    println(zipWith(xs, ys)(_ + _))     // List(7, 10, 12)
    println(zipWith(List("hola", "hello"), List("chico", "world"))(_.concat(_)))    // List("holachico", "helloworld")
  }
}
