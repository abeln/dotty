
import scala.ExplicitNulls.ArrayUtils.ArrayExtensions

// Test that the new copyOf method for arrays can (unsoundly) copy
// non-nullable arrays.
object Test {
  def nonNullArray(): Unit = {
    val arr = Array("hello", "world")
    val arr2: Array[String] = Array.copyOf(arr, arr.length)
    arr(0) = "hello2"
    arr(1) = "world2"
    assert(arr2(0) == "hello")
    assert(arr2(1) == "world")
  }

  def nullableArray(): Unit = {
    val arr = Array[String|Null]("hello", "world")
    val arr2: Array[String|Null] = Array.copyOf(arr, arr.length)
    arr(0) = "hello2"
    arr(1) = "world2"
    assert(arr2(0) == "hello")
    assert(arr2(1) == "world")
  }
  
  def arrayWithNull(): Unit = {
    val arr = Array[String](null.asInstanceOf[String], "world")
    val arr2: Array[String] = Array.copyOf(arr, arr.length)
    arr(0) = "hello2"
    arr(1) = "world2"
    assert(arr2(0) == null)
    assert(arr2(1) == "world")
  }

  def main(args: Array[String]): Unit = {
    nonNullArray()
    nullableArray()
    arrayWithNull()
 }
}
