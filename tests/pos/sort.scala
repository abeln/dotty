object sorting {

  import scala.ExplicitNulls.ArrayUtils._

  val xs: Array[String] = ???

  java.util.Arrays.sort(xs.withNullElems, ???)

}
