package net.zelinf.cryptohw.math

object Unsigned {

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
}
