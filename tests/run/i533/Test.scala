object Test {
  def main(args: Array[String]): Unit = {
    val x = new Array[Int|Null](1)
    x(0) = 10
    println(JA.get(x))
  }
}
