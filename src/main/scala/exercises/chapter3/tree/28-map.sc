import examples.chapter3_functional_data_structures.{Branch, Leaf, Tree}

def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
  t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }


val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(3), Branch(Leaf(4), Leaf(2))))
map(tree)(_ + 1)   // Branch(Branch(Leaf(2),Leaf(6)),Branch(Leaf(4),Branch(Leaf(5),Leaf(3))))