package net.zelinf.cryptohw.math

import scala.language.implicitConversions

final class GFBit(val value: Boolean) extends AnyVal {}

object GFBit {

  implicit def booleanToGFBit(value: Boolean): GFBit = {
    new GFBit(value)
  }

  implicit def gfBitToBoolean(wrapper: GFBit): Boolean = wrapper.value

  implicit val gfBitField: Field[GFBit] = new Field[GFBit] {
    override def one: GFBit = true
    override def multiply(x: GFBit, y: GFBit): GFBit = x & y
    override def mulInvert(x: GFBit): GFBit = x
    override def eqv(x: GFBit, y: GFBit): Boolean = x == y
    override def empty: GFBit = false
    override def combine(x: GFBit, y: GFBit): GFBit = x ^ y
  }
}
