object Test {
  def main(args: Array[String]): Unit = {
    import scala.ExplicitNulls.ArrayConversions._
    J_2.main(args.withNullElems)
  }
}
