

// Show that we can invoke compat mode in a specific scope.
class Foo {

  def foo(): Unit = {
    import scala.language.implicitNulls
    val x: String|Null = ???
    val x2: String = x
    val x3: String = null
  }

  def foo2(): Unit = {
    val x: String|Null = ???
    val x2: String = x    // error
    val x3: String = null // error
  }
}
