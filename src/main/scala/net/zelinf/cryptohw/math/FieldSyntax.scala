package net.zelinf.cryptohw.math
import cats.syntax.MonoidSyntax

object FieldSyntax extends MonoidSyntax {
  implicit final class FieldOps[A: Field](lhs: A) {
    def |*|(rhs: A): A =
      Field.multiply(lhs, rhs)
  }
}
