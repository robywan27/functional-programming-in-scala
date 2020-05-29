package chapters.chapter5_laziness

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  // stack-safe implementation
  /*{
    @scala.annotation.tailrec
    def helper(s: Stream[A], l: List[A]): List[A] =
      s match {
        case Empty => l
        case Cons(h, t) => helper(t(), l :+ h())
      }
    helper(this, List[A]())
  }*/

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  // stack-safe implementation
  /*{
    @scala.annotation.tailrec
    def helper(s: Stream[A], n: Int): Stream[A] =
      if (n == 0) s
      else s match {
        case Empty => empty
        case Cons(_, t) => helper(t(), n - 1)
      }
    helper(this, n)
  }*/

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def exists(p: A => Boolean): Boolean =
    /*this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }*/
    foldRight(false) ((x, xs) => p(x) || xs)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  def headOptionWithFoldRight: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](m: => Stream[B]): Stream[B] =
    foldRight(m)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
        case (Cons(h, t), i) if i > 0 => Some(h(), (t(), i - 1))
        case _ => None
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    !zipWith(s)(_ == _).exists(_ == false)
    /*zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }*/

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(Cons(h, t), t())  // alternative solution: case s => Some((s, s drop 1))
    } append Stream(empty)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) ((a, t) => {
      lazy val lt = t
      val b = f(a, lt._1)
      (b, cons(b, lt._2))
    })._2
}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]



object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  lazy val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(m: Int, n: Int): Stream[Int] =
      cons(m, loop(n, m + n))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  lazy val onesWithUnfold: Stream[Int] =
    unfold(1)(one => Some(one, one))

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a)(a => Some(a, a))

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(0)(n => Some(n + 1, n + 1))

  def fibsWithUnfold: Stream[Int] =
    unfold((0, 1)) {case (m, n) => Some(m, (n, m + n))}


  def main(args: Array[String]): Unit = {
    print("toList: ")
    println(Stream(1, 2, 3).toList)                                 // toList: List(1, 2, 3)
    print("take: ")
    println(Stream(1, 2, 3).take(2).toList)                         // take: List(1, 2)
    print("drop: ")
    println(Stream(1, 2, 3).drop(2).toList)                         // drop: List(3)
    print("takeWhile: ")
    println(Stream(1, 3, 7, 4, 9, 2).takeWhile(_ % 2 == 1).toList)  // takeWhile: List(1, 3, 7)
    println(); println("forAll")
    println(Stream(2, 4, 6, 8).forAll(_ % 2 == 0))                  // true
    println(Stream(2, 4, 7, 8).forAll(_ % 2 == 0))                  // false
    println(); println("takeWhile using foldRight")
    println(Stream(1, 3, 7, 4, 9, 2).takeWhileWithFoldRight(_ % 2 == 1).toList)  // List(1, 3, 7)
    println(); println("headOption using foldRight")
    println(Stream(1, 2, 3).headOptionWithFoldRight)                             // Some(1)
    println(Empty.headOptionWithFoldRight)                                       // None
    println()
    println(s"map: ${Stream(1, 2, 3).map(_ * 4).toList}")                         // map: List(4, 8, 12)
    println(s"filter: ${Stream(1, 2, 3, 4, 5).filter(_ % 2 == 1).toList}")        // filter: List(1, 3, 5)
    println(s"append: ${Stream(1, 2, 3).append(Stream(4, 5, 6, 7)).toList}")      // append: List(1, 2, 3, 4, 5, 6, 7)
    println(s"flatMap: ${Stream(1, 2, 3).flatMap(e => Stream(e, e + 1)).toList}") // flatMap: List(1, 2, 2, 3, 3, 4)

    println(); println("Infinite streams")
    println(ones.take(5).toList)                  // List(1, 1, 1, 1, 1)
    println(ones.exists(_ % 2 != 0))              // true
    println(ones.map(_ + 1).exists(_ % 2 == 0))   // true
    println(ones.takeWhile(_ == 1))               // returns a stream - it doesn't overflow the stack because it is lazy; if you call for example toList on it it will crash
    println(ones.forAll(_ != 1))                  // false
    println()

    println(s"map using unfold: ${Stream(1, 2, 3).map(_ * 4).toList}")                                      // map using unfold: List(4, 8, 12)
    println(s"take using unfold: ${Stream(1, 2, 3).take(2).toList}")                                        // take using unfold: List(1, 2)
    println(s"takeWhile using unfold: ${Stream(1, 3, 7, 4, 9, 2).takeWhile(_ % 2 == 1).toList}")            // takeWhile using unfold: List(1, 3, 7)
    println(s"zipWith: ${Stream("hola", "hello").zipWith(Stream("chico", "world"))(_.concat(_)).toList}")   // zipWith: List(holachico, helloworld)
    println(s"zipAll: ${Stream("hola", "hello", "ciao").zipAll(Stream("chico", "world")).toList}")         // zipWAll: List((Some(hola),Some(chico)), (Some(hello),Some(world)), (Some(ciao),None))
    println()
    println(s"startsWith: ${Stream(1, 2, 3).startsWith(Stream(1, 2))}")                              // true
    println(s"startsWith: ${Stream("hola", "hello", "ciao").startsWith(Stream("hola", "hello"))}")   // true
    println(s"startsWith: ${Stream("hola", "hello", "ciao").startsWith(Stream("hola", "hola"))}")    // false
    println(s"tails: ${Stream(1, 2, 3).tails.toList.map(_.toList)}")                                 // tails: List(List(1, 2, 3), List(2, 3), List(3), List())
    println(s"scanRight: ${Stream(1,2,3).scanRight(0)(_ + _).toList}")                               // scanRight: List(6, 5, 3, 0)
  }
}
