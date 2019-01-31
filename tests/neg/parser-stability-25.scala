class A extends (Int => i1) // error
class B extends (Int => this) // error
trait C {
  val bar: Int => this // error
}

// Test that function types ending in SIP-23 singleton types are understood correctly.

class D extends (Int => 1) {
  def apply(x: Int) = 2 // error
}

class Wrap(x: Int)
class E extends (Wrap)( // error

class Foo // error
// If we don't add code below `E`, then 
// the error is reported on the following line,
// but we can't annotate it because by adding a new line
// we shift the error one line down.
