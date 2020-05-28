// methods
def abs(n: Int): Int =
  if (n < 0) -n	// if is an expression
  else n

private def formatAbs(n: Int) = {
  val msg = "The absolute value of %d is: %d"	// val -> immutable
  msg.format(n, abs(n))
}

// recursive functions
def fact(n: Int): Int = {
  @annotation.tailrec
  def factHelper(n: Int, acc: Int): Int =
    if (n <= 0)   acc
    else factHelper(n - 1, n * acc)

  factHelper(n, 1)
}

// higher-order functions
def formatResult(name: String, n: Int, f: Int => Int): String = {
  val msg = "The %s of %d is: %d"
  msg.format(name, n, f(n))
}


println(formatResult("absolute value", -42, abs)) // The absolute value of -42 is: 42
println(formatResult("factorial", 5, fact))       // The factorial of 5 is: 120



/*
 Exercise 2.1
*/
def fib(n: Int): Int = {
  @annotation.tailrec
  def fibHelper(n: Int, firstVal: Int, secondVal: Int): Int =
    if (n == 0)   firstVal
    else fibHelper(n - 1, secondVal, firstVal + secondVal)

  fibHelper(n, 0, 1)
}

println(fib(10))                     // 55




/*
 polymorphic functions
 */
def findFirstString(ss: Array[String], key: String): Int = {
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

println(findFirstString(Array("ciao", "hello", "hola"), "hello"))   // 1
println(findFirst(Array(12, 55, 2, 7), (n: Int) => n == 2))               // 2


/*
 Exercise 2.2
 */
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

/*private*/ def smallerThan(x: Int, y: Int): Boolean =
  if (x < y)  true
  else false

println(isSorted(Array(1, 3, 2, 7, 6, 8), smallerThan))   // false
println(isSorted(Array(1, 2, 3, 6, 7, 8), smallerThan))   // true




/*
 Exercise 2.3
 */
def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)
def sum(x: Int, y: Int): Int =
  x + y

val curriedSum = curry(sum)
println(curriedSum(2)(3))            // 5




/*
 Exercise 2.4
 */
def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}
def currySum(x: Int): Int => Int =
  y => x + y

val uncurriedSum = uncurry(currySum)
println(uncurriedSum(7, 8))          // 15




/*
 Exercise 2.5
 */
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a: A => f(g(a))
}
val square: Int => Int = (x: Int) => x * x
val decrement: Int => Int = x => x - 1

println(square(decrement(7)))                // 36
println(compose(square, decrement)(7))  // 36

// andThen
println(decrement.andThen(square)(7))        // 36


