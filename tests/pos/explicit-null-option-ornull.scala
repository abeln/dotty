

// Test that Option.orNull correctly infers the type argument `String|Null`
class Foo {

  type Nullable[T] = T|Null
  val x: Option[String] = None
  val y: Nullable[String] = x.orNull

}
