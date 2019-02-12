
// Check that we ignore nullability during override checks.
trait Base {
  def foo(x: String): Unit = ???
  def foo2(x: String|Null): Unit = ???
  def foo3(): String|Null = ???
  def foo4(): String = ???

  def bar(x: Array[String]): Unit = ???
  def bar2(x: Array[String|Null]|Null): Unit = ???
  def bar3(): Array[String|Null] = ???
  def bar4(): Array[String] = ???
}

trait Derived extends Base {
  override def foo(x: String|Null): Unit = ???
  override def foo2(x: String): Unit = ???
  override def foo3(): String = ???
  override def foo4(): String|Null = ???

  override def bar(x: Array[String|Null]|Null): Unit = ???
  override def bar2(x: Array[String]): Unit = ???
  override def bar3(): Array[String] = ???
  override def bar4(): Array[String|Null]|Null = ???
}
