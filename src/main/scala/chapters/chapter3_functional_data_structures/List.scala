package chapters.chapter3_functional_data_structures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

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
    if (n <= 0) l
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

  def foldRight[A, B](as: List[A], v: B)(f: (A, B) => B): B =
    as match {
      case Nil => v
      case Cons(x, xs) => f(x, foldRight(xs, v)(f))
    }

  def sumWithFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def productWithFoldRight(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc)

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], v: B)(f: (B, A) => B): B =
    as match {
      case Nil => v
      case Cons(x, xs) => foldLeft(xs, f(v, x))(f)
    }

  def sumWithFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productWithFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthWithFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((xs, x) => Cons(x, xs))

  def appendWithFold[A](l: List[A], m: List[A]): List[A] =
    foldRight(l, m)(Cons(_, _))

  def concat[A](xss: List[List[A]]): List[A] =
    foldRight(xss, List[A]()) (append)

  def increment(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, acc) => Cons(1 + x, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, acc) => if (p(x)) Cons(x, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((x, xs) => append(f(x), xs))

  def filterWithFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(x => if (p(x)) List(x) else Nil)

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

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @scala.annotation.tailrec
    def startsWith(sup: List[A], sub: List[A]): Boolean =
      (sup, sub) match {
        case (_, Nil) => true
        case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
        case _ => false
      }
    sup match {
      case Nil => sub == Nil
      case Cons(x, xs) if startsWith(Cons(x, xs), sub) => true
      case Cons(_, xs) => hasSubsequence(xs, sub)
    }
  }
}
