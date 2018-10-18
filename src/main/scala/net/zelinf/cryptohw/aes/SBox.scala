package net.zelinf.cryptohw.aes

import net.zelinf.cryptohw.math._
import BitUtils._

private[aes] object SBox {

  private val c: Byte = BigInt("01100011", 2).toByte

  def subByte(byte: Byte): Byte = {
    val z = mulInvertForInput(byte)

    def getBit(byte: Byte, index: Int): Byte = {
      if ((byte & (1 << index)) != 0) 1
      else 0
    }

    (for (i <- 0 to 7) yield {
      val bi = (List(0, 4, 5, 6, 7)
        .map(j => getBit(z, (i + j) % 8))
        .sum + getBit(c, i)) % 2
      (bi << i).toByte
    }).foldLeft(0.toByte)((x, y) => (x | y).toByte)
  }

  def invSubByte(byte: Byte): Byte = {
    val z = mulInvertForInput(byte)
    val coef: Array[Byte] = Array(
      b"00100101",
      b"10010010",
      b"01001001",
      b"10100100",
      b"01010010",
      b"00101001",
      b"10010100",
      b"01001010"
    )
    var result: Byte = 0
    for (i <- coef.indices) {
      result = bitwiseOr(
        result,
        (booleanToInt(bitCount(bitwiseAnd(coef(i), z)) % 2 != 0) << i).toByte
      )
    }
    result = bitwiseXor(result, b"10100000")
    result
  }

  private def mulInvertForInput(byte: Byte): Byte = {
    if (byte != 0)
      Field.mulInvert(GF2Elem(byte)).value.toByte
    else 0
  }

}
