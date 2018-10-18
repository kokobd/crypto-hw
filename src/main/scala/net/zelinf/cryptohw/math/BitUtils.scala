package net.zelinf.cryptohw.math

object BitUtils {

  def unsignedByteToWord(unsignedByte: Byte): Int = {
    unsignedByte & 0xFF
  }

  def unsignedBytesToWord(bytes: Array[Byte]): Int = {
    require(bytes.length == 4)
    var word = 0
    for (i <- 0 to 3) {
      word |= unsignedByteToWord(bytes(i)) << (i * 8)
    }
    word
  }

  def wordToUnsignedBytes(word: Int): Array[Byte] = {
    val bytes = new Array[Byte](4)
    for (i <- 0 to 3) {
      bytes(i) = ((word >>> i * 8) & 0xFF).toByte
    }
    bytes
  }

  def bitCount(value: Long): Int = {
    java.lang.Long.bitCount(value)
  }

  def bitCount(value: Byte): Int = {
    bitCount(unsignedByteToWord(value))
  }

  implicit class BinaryStringInterpolation(val sc: StringContext)
      extends AnyVal {
    def b(args: Any*): Byte = {
      BigInt.apply(sc.parts.head, 2).toByte
    }
  }

  def bitwiseAnd(x: Byte, y: Byte): Byte = {
    (x & y).toByte
  }

  def booleanToInt(x: Boolean): Int = {
    if (x) 1 else 0
  }

  def bitwiseOr(x: Byte, y: Byte): Byte = {
    (x | y).toByte
  }

  def bitwiseXor(x: Byte, y: Byte): Byte = (x ^ y).toByte
}
