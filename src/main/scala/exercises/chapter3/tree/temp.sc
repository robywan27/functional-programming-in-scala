import examples.chapter3_functional_data_structures.{Branch, Leaf, Tree}

/*def fold[A, B](t: Tree[A], v: B)(f: (A, B) => B): B =
  t match {
    case Leaf(v) =>
    case Branch(l, r) =>
  }*/


val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
