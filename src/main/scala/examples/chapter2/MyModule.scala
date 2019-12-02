package examples.chapter2

/**
 * MyModule object module
 */
object MyModule { // singleton object declaring class and object
  /*
      examples of methods
   */
  def abs(n: Int): Int =
    if (n < 0) -n	// if is an expression
    else n

  private def formatAbs(n: Int) = {
    val msg = "The absolute value of %d is: %d"	// val -> immutable
    msg.format(n, abs(n))
  }

  /*
      recursive functions
   */
  def fact(n: Int): Int = {
    @annotation.tailrec
    def factHelper(n: Int, acc: Int): Int =
      if (n <= 0)   acc
      else factHelper(n - 1, n * acc)

    factHelper(n, 1)
  }

  def fact2(n: Int): Int = {
    @annotation.tailrec
    def factHelper(value: Int, counter: Int): Int =
      if (counter == 1)   value
      else factHelper(value * counter, counter - 1)

    factHelper(n, n - 1)
  }

  /*
      higher-order functions
   */
  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is: %d"
    msg.format(name, n, f(n))
  }

  /*
      polymorphic functions
   */
  def findFirst(ss: Array[String], key: String): Int = {
    val n = ss.length
    @annotation.tailrec
    def looper(i: Int): Int =
      if (i >= n)   -1
      else if (key == ss(i))  i
      else looper(i + 1)

    looper(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    val n = as.length
    @annotation.tailrec
    def looper(i: Int): Int =
      if (i >= n)   -1
      else if (p(as(i)))  i
      else looper(i + 1)

    looper(0)
  }

  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibHelper(n: Int, firstVal: Int, secondVal: Int): Int =
      if (n == 0)   firstVal
      else fibHelper(n - 1, secondVal, firstVal + secondVal)

    fibHelper(n, 0, 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val n = as.length
    @annotation.tailrec
    def looper(i: Int): Boolean = {
      if (i >= n - 1)   true
      else if (!ordered(as(i), as(i + 1)))   false
      else looper(i + 1)
    }

    looper(0)
  }
  private def smallerThan(x: Int, y: Int): Boolean =
    if (x < y)  true
    else false

  /*
    curried parameters
   */
  def sum(x: Int, y: Int): Int =
    x + y
  def currySum(x: Int): Int => Int =
    (y) => x + y

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // Exercise 2.4
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def square(x: Int): Int = x * x
  def decrement(x: Int): Int = x - 1


  // main method: every class/object must contain one with this precise signature to be executable
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs)) // The absolute value of -42 is: 42
    println(formatResult("factorial", 5, fact))       // The factorial of 5 is: 120
    println(findFirst(Array("ciao", "hello", "hola"), "hello"))
    println(findFirst(Array("ciao", "hello", "hola"), "hell"))

    // Exercise 2.1
    println(fib(10))                     // 55
    // Exercise 2.2
    println(isSorted(Array(1, 3, 2, 7, 6, 8), smallerThan))   // false
    println(isSorted(Array(1, 2, 3, 6, 7, 8), smallerThan))   // true
    // Exercise 2.3
    val curriedSum = curry(sum)
    println(curriedSum(2)(3))            // 5
    // Exercise 2.4
    val uncurriedSum = uncurry(currySum)
    println(uncurriedSum(7, 8))          // 15
    // Exercise 2.5
    val squareDecrement = compose(square, decrement)
    println(square(decrement(7)))        // 36
    println(squareDecrement(7))          // 36
  }
}
