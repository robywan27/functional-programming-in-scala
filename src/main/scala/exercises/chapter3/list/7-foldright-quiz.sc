// Can product, implemented using foldRight, immediately halt the recursion and
//return 0.0 if it encounters a 0.0?

// Answer: No, because foldRight is evaluated strictly instead of lazily. This means that at each stage the function is
// invoked, it immediately evaluates its arguments.
