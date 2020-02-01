import examples.chapter3_functional_data_structures.{Branch, Leaf, Tree}

def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
  t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }