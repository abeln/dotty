
import scala.language.implicitNulls

// Test the language import for implicit nulls.
// When the import is in scope, two kinds of implicit conversions
// are enabled:
//   1) null => null.asInstanceOf[Prototype]
//   2) x: T|Null => x.asInstanceOf[T]
class Foo {
  val x: String = null // ok: conversion added

  val y: String|Null = "hello"
  val y2: String = y // ok: conversion added

  def bar(s: String): String = s

  bar(Lib.foo())

  def foo[T <: AnyRef](x: T|Null): T = {
    x
  }

  val xx: (String, String) = (null, null)
  val null2: (Null, Null) = (null, null)
}

object Lib {
  def foo(): String|Null = "hello"
}

object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo()
  }
}
