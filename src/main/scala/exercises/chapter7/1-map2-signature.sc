import chapters.chapter7_parallelism.Par.Par

// provide the most general signature for Par.map2
def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???