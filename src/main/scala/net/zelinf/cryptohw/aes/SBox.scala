package net.zelinf.cryptohw.aes
import net.zelinf.cryptohw.math.{Field, GF2Elem}

private[aes] object SBox {

  private val c: Byte = BigInt("01100011", 2).toByte

  def subByte(byte: Byte): Byte = {
    val z: Byte =
      if (byte != 0)
        Field.mulInvert(GF2Elem(byte)).value.toByte
      else 0

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
}
