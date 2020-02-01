package examples.chapter3_functional_data_structures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l , r) => maximum(l).max(maximum(r))
    }

  /*def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l , r) => depth(l) + depth(r)
    }*/

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }




  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
    // test Exercise 3.25
    println(size(tree))       // 9 (5 leaves, 4 branches
    // test Exercise 3.26
    println(maximum(tree))    // 5

    // test Exercise 3.28
    println(map(tree)(_ + 5))   // Branch(Branch(Leaf(6), Leaf(7)), Branch(Leaf(8), Branch(Leaf(9), Leaf(10))))
  }
}