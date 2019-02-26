package scala

import scala.reflect.ClassTag

object ExplicitNulls {
  implicit class NonNull[T](x: T|Null) extends AnyVal {
    def nn: T = if (x == null) {
      throw new NullPointerException("tried to cast away nullability, but value is null")
    } else {
      x.asInstanceOf[T]
    }
  }

  object ArrayUtils {
    // In the conversion methods below, the type arguments need to be reference types, since
    // otherwise `Array[T]` and `Array[T|Null]` will have different erasures.

    implicit class ToNullable1[T <: AnyRef](arr: Array[T]) extends AnyVal {
      def withNullElems: Array[T|Null] = arr.asInstanceOf[Array[T|Null]]
    }

    implicit class FromNullable1[T <: AnyRef](arr: Array[T|Null]) extends AnyVal {
      def withNonNullElems: Array[T] = arr.asInstanceOf[Array[T]]
    }

    /** Extension methods for the `Array` companion object.
     *  TODO(abeln): move to `Array` object in the standard library.
     */
    implicit class ArrayExtensions(arr: Array.type) extends AnyVal {
      def ofNulls[T >: Null](dim: Int)(implicit classTag: ClassTag[T]): Array[T] = {
        Array.ofDim(dim)
      }

      def ofZeros[T <: AnyVal](dim: Int)(implicit classTag: ClassTag[T]): Array[T] = {
        Array.ofDim(dim)
      }

      /** A Scala version of j.u.Arrays.copyOf, that copies arrays of non-nullable elements.
       *  e.g. we can pass `Array[String]` and get back `Array[String]` to and from this method,
       *  but can't do the same with the Java one (because it only accepts and returns arrays of
       *  nullable elements).
       */
      def copyOf[T <: AnyRef|Null](original: Array[T], newLength: Int): Array[T] = {
        java.util.Arrays.copyOf(original.asInstanceOf[Array[T|Null]], newLength).nn.asInstanceOf[Array[T]]
      }
    }
  }
}
