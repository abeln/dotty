class Foo {
  val s: String = ???
  s match {
    case x: String => 1
    // Test that this isn't marked as unreachable (because pattern matching) type tests
    // use isInstanceOf and null.isInstanceOf[T] returns false for all T.
    case null => 2 
  }
}
