
import scala.language.implicitNulls

class Foo {
  def foo(): String|Null = "hello"

  // Test that in backwards compat mode the return value
  // of `foo()` is correctly coerced to `String`.
  val x: Option[String] = Option(foo())
}
