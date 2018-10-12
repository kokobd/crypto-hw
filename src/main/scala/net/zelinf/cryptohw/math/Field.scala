package net.zelinf.cryptohw.math

import cats._

trait Field[A] extends Monoid[A] with Eq[A] {

  def one: A

  def multiply(x: A, y: A): A

  def mulInvert(x: A): A
}

object Field {

  def apply[A: Field]: Field[A] = implicitly[Field[A]]

  def zero[A: Field]: A = Field[A].empty

  def one[A: Field]: A = Field[A].one

  def sum[A: Field](x: A, y: A): A = Field[A].combine(x, y)

  def multiply[A: Field](x: A, y: A): A = Field[A].multiply(x, y)

  def mulInvert[A: Field](x: A): A = Field[A].mulInvert(x)
}
