package scala

object ExplicitNulls {
  // This version of the definition can't use union types because
  // it's compiled by scalac. We make it a no-op instead.
  type Nullable[T] = T
}
