case class Foo()

object Test {
  def main(args: Array[String]): Unit = {
    assert(Foo.unapply(Foo()) == true)

    // The line below no longer typechecks, since null is no
    // longer a subtype of `Foo`.
    // assert(Foo.unapply(null) == true)

    Foo() match {
      case Foo() => ()
      case _     => ???
    }

    Foo() match {
      case _: Foo => ()
      case _      => ???
    }

    (Foo(): Any) match {
      case Foo() => ()
      case _     => ???
    }
  }
}
