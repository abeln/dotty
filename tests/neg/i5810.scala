import language.strictEquality
def f[T](x: T) =
  if (x == null) ???        // ok: comparison with `null` allowed
  else if (x == "abc") ???  // error: cannot be compared
  else ???
