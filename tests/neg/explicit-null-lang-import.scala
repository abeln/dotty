
import scala.language.implicitNulls

// Test the language import for implicit nulls.
// When the import is in scope, two kinds of implicit conversions
// are enabled:
//   1) null => null.asInstanceOf[Prototype]
//   2) x: T|Null => x.asInstanceOf[T], if T <: AnyRef
class Foo {
  val x: String = null // ok: conversion added

  val y: String|Null = "hello"
  val y2: String = y // ok: conversion added

  val x2: Int = null // error: Int is not a reference type
  val y3: Int|Null = 42
  val y4: Int = y3 // error: Int|Null not a subtype of AnyRef, so the conversion
                   // doesn't kick in

  def foo[T <: AnyRef](x: T|Null): T = {
    val x2: T = x // ok: T <: AnyRef
    null          // ok: T <: AnyRef
  }

  def bar[T](x: T|Null): T = {
    val x2: T = x // error
    null          // error
  }

  val xx: (String, String) = null // error: we only cast at the outermost level
  val nullList: Array[String] = (??? : Array[String|Null]) // error: array is invariant and we only cast at the outermost level
}
