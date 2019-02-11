
// Test sound array creation extension methods.
class Foo {

  import scala.ExplicitNulls.ArrayUtils._
  val r1: Array[String] = new Array[String](10)

  val r2: Array[String] = Array.ofNulls(10) // error: type parameter for ofNull needs to be nullable type
  val r3: Array[String] = Array.ofNulls[String|Null](10)  // error: type parameter for ofNull needs to be nullable type
  val r4: Array[String|Null] = Array.ofNulls(10)
  val r5: Array[String|Null] = Array.ofNulls[String|Null](20)
  val r6: Array[Int|Null] = Array.ofNulls(50)

  val v1: Array[Int] = Array.ofZeros(10) 
  val v3: Array[String] = Array.ofZeros(20) // error: must pass value type
  val v4: Array[Boolean] = Array.ofZeros(10)
  val v5: Array[Unit] = Array.ofZeros(20)
  val v6: Array[Float] = Array.ofZeros(30)
}
