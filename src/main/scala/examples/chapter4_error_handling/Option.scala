package examples.chapter4_error_handling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(v) => v
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    // first apply map using f - return type: Option[Option[B]]
    // use getOrElse to unpack the nested Option
    map(f) getOrElse None
    // with pattern matching
    /*this match {
      case None => None
      case Some(v) => f(v)
    }*/

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    // first apply map using a function which converts input value to Some(value) - returns Option[Some[A]]
    // use getOrElse to unpack the nested option
    this.map (Some(_)) getOrElse ob
    // with pattern matching
    /*this match {
      case None => ob
      case Some(_) => this
    }*/

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
    // with pattern matching
    /*this match {
      case Some(v) if f(v) => Some(v)
      case _ => None
    }*/
}


case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]



object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)   None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft[Option[List[A]]] (Some(Nil)) ((xs, x) => map2(x, xs)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft[Option[List[B]]] (Some(Nil)) ((xs, x) => map2(f(x), xs)(_ :: _))


  def main(args: Array[String]): Unit = {
    println("Mean")
    println(mean(Seq()))                      // None
    println(mean(Seq(1.0, 2.0, 0.0, 5.0)))    // Some(2.0)
    println()

    val possiblySomeValue = Some(2)
    val noneValue: Option[Int] = None

    println("Map")
    println(possiblySomeValue.map(_ * 3))   // Some(6)
    println(noneValue.map(_ * 3))           // None
    println()
    println("getOrElse")
    println(possiblySomeValue.getOrElse(4))   // 2
    println(noneValue.getOrElse(4))           // 4
    println()
    println("flatMap")
    println(possiblySomeValue.flatMap((x: Int) => if (x <= 0) Some(x) else None))  // None
    println(noneValue.flatMap((x: Int) => if (x > 0) Some(x) else None))           // None
    println()
    println("orElse")
    println(possiblySomeValue.orElse(Some(4)))  // Some(2)
    println(noneValue.orElse(Some(4)))          // Some(4)
    println()
    println("filter")
    println(possiblySomeValue.filter(_ % 2 == 1))   // None
    println(noneValue.filter(_ % 2 == 0))           // None
    println()

    println("Lifting")
    val abs0: Option[Int] => Option[Int] = lift(math.abs)
    println(abs0(Some(-2)))   // Some(2)
    println()


  }
}
