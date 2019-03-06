package scales.xml

trait PullType
class QName
trait RetUrn[T]

/**
 * Iteratees related to pull parsing
 */
trait PullIteratees {
  /**
   * Without the overload it doesn't trigger the CCE, even though its
   * not used
   */
  def iterate(path: List[QName], xml: String): RetUrn[String] = new RetUrn[String] {}
  def iterate(path: List[QName], xml: Iterator[PullType]): RetUrn[String] = new RetUrn[String] {}
}
