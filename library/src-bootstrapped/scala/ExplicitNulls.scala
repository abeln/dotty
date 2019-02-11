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

    implicit class Factory(arr: Array.type) extends AnyVal {
      def ofNulls[T >: Null](dim: Int)(implicit classTag: ClassTag[T]): Array[T] = {
        Array.ofDim(dim)
      }

      def ofZeros[T <: AnyVal](dim: Int)(implicit classTag: ClassTag[T]): Array[T] = {
        Array.ofDim(dim)
      }
    }
  }
}
