// TODO(abeln): remove casts after standard library is migrated to explicit null
class Foo extends scala.collection.SeqView[Int, List[Int]] {
  def iterator: Iterator[Int] = null.asInstanceOf[Iterator[Int]]
  def apply(idx: Int): Int = idx
  def length: Int = 0
  protected def underlying = null.asInstanceOf[List[Int]]
}

object Test {
  def main(args: Array[String]): Unit = {
    val f: scala.collection.TraversableViewLike[Int, List[Int], _] = new Foo
    new f.Transformed[Int] {
      def foreach[U](f: Int => U): Unit = ()
      // underlying is a lazy val
      assert(underlying == null)
    }
  }
}
