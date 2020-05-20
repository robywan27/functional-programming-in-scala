import examples.chapter3_functional_data_structures.{Branch, Leaf, Tree}

def fold[A, B](t: Tree[A])(f: A => B)(m: (B, B) => B): B =
  t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => m(fold(l)(f)(m), fold(r)(f)(m))
  }


def sizeWithFold[A](t: Tree[A]): Int =
  fold(t)(_ => 1)(1 + _ + _)

def maximumWithFold(t: Tree[Int]): Int =
  fold(t)((x: Int) => x)(_ max _)

def depthWithFold[A](t: Tree[A]): Int =
  fold(t)(_ => 1)(1 + _ max _)

// Note: add annotation for return type of f, otherwise is inferred as Leaf[B]!
def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t) (v => Leaf(f(v)): Tree[B]) (Branch(_, _))



val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
fold(tree)(_ + 1)(_ + _)   // 20

sizeWithFold(tree)         // 9
maximumWithFold(tree)      // 5
depthWithFold(tree)        // 4
mapWithFold(tree)(_ + 1)   // Branch(Branch(Leaf(2),Leaf(6)),Branch(Leaf(4),Branch(Leaf(5),Leaf(3))))