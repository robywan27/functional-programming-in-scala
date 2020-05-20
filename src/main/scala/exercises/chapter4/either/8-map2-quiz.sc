import examples.chapter4_error_handling._

/*case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))
def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))
def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person)*/



// What would you need to change in order to report both errors?
  // Would you change map2 or the signature of mkPerson?
  // Or could you create a new data type that captures this requirement better than Either does, with some additional structure?
  // How would orElse, traverse, and sequence behave differently for that data type?


// I would define a data type similar to Either that instead of having a single value for Left would have a list containing all possible errors
sealed trait Partial[+E, +A]
case class Errors[+E] (values: List[E]) extends Partial[List[E], Nothing]
case class Right[+A] (value: A) extends Partial[Nothing, A]

