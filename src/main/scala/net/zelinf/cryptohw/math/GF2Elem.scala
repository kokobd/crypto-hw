package net.zelinf.cryptohw.math

import cats.Eq

class GF2Elem private (val value: BigInt) {}

object GF2Elem {

  def apply(value: BigInt): GF2Elem = new GF2Elem(value)

  def apply(unsignedByte: Byte): GF2Elem =
    GF2Elem(BigInt(BitUtils.unsignedByteToWord(unsignedByte)))

  implicit val instanceEq: Eq[GF2Elem] =
    (x: GF2Elem, y: GF2Elem) => x.value == y.value

  def newGF2ElemFieldInstance(rp: BigInt): Field[GF2Elem] =
    new Field[GF2Elem] {
      override def one: GF2Elem = GF2Elem(1)
      override def multiply(x: GF2Elem, y: GF2Elem): GF2Elem =
        GF2Elem(mod(GF2Elem.multiply(x.value, y.value), rp))
      override def mulInvert(x: GF2Elem): GF2Elem =
        GF2Elem(GF2Elem.mulInvert(x.value, rp))
      override def empty: GF2Elem = GF2Elem(0)
      override def combine(x: GF2Elem, y: GF2Elem): GF2Elem =
        GF2Elem(mod(sum(x.value, y.value), rp))
      override def eqv(x: GF2Elem, y: GF2Elem): Boolean = instanceEq.eqv(x, y)
    }

  private def mod(num: BigInt, den: BigInt): BigInt = {
    var num_ = num
    while (num_.bitLength >= den.bitLength) {
      num_ ^= (den << (num_.bitLength - den.bitLength))
    }
    num_
  }

  private def sum(x: BigInt, y: BigInt): BigInt = x ^ y

  private def multiply(x: BigInt, y: BigInt): BigInt = {
    var result = BigInt(0)
    for (i <- 0 until y.bitLength) {
      if (y.testBit(i)) {
        result ^= (x << i)
      }
    }
    result
  }

  private def mulInvert(x: BigInt, rp: BigInt): BigInt = {
    var b: BigInt = 1
    var c: BigInt = 0
    var u = x
    var v = rp

    def degree(x: BigInt): Int = if (x.bitLength > 0) x.bitLength - 1 else 0

    while (degree(u) != 0) {
      var j = degree(u) - degree(v)
      if (j < 0) {
        // swap u, v
        var tmp = u
        u = v
        v = tmp
        // swap b, c
        tmp = b
        b = c
        c = tmp

        j = -j
      }

      val x = BigInt(0).setBit(j)
      u = mod(sum(u, multiply(v, x)), rp)
      b = mod(sum(b, multiply(c, x)), rp)
    }
    b
  }

}
