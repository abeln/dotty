class A[T <: A[T]] {

}

object Test {
  val x: A[_] = ???
}
