
// Test sound array creation extension methods.
class Foo {

  import scala.ExplicitNulls.ArrayUtils._
  val v1: Array[Int|Null] = Array.ofZeros(10) // error
}
