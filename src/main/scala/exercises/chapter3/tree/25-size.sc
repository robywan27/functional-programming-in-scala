import chapters.chapter3_functional_data_structures.{Branch, Leaf, Tree}

def size[A](t: Tree[A]): Int =
  t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }


val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
size(tree)     // 9