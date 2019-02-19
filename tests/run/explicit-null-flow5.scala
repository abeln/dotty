// Test that flow-sensitive type inference handles
// early exists from blocks.
object Test {
  def main(args: Array[String]): Unit = {
    check("hello")
    check("world")
    try {
      check(null)
    } catch {
      case npe: NullPointerException =>
        println("npe")
    }
  }
  
  def err(msg: String) = throw new NullPointerException(msg)

  def check(s: String|Null): String = {
    if (s == null) err("null argument!")
    s
  }
}
