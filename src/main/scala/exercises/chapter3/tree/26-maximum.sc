import examples.chapter3_functional_data_structures.{Branch, Leaf, Tree}

def maximum(t: Tree[Int]): Int =
  t match {
    case Leaf(v) => v
    case Branch(l , r) => maximum(l).max(maximum(r))
  }


val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(3), Branch(Leaf(4), Leaf(2))))
println(maximum(tree))    // 5