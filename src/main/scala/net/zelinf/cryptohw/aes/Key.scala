package net.zelinf.cryptohw.aes
import net.zelinf.cryptohw.math.BitUtils

final class Key private (private val data: Array[Int]) {

  lazy val rounds: Int = Key.lengthToRounds(data.length * 4)

  def roundKeys: Seq[Key] = {
    val expansion = new Array[Int](4 * rounds)
    val n = data.length
    for (i <- expansion.indices) {
      expansion(i) = {
        if (i < n) {
          data(i)
        } else if (i > n && i % n == 0) {
          expansion(i - n) ^
            Key.rotWord(Key.subWord(expansion(i - 1))) ^
            Key.rcon(i / n)
        } else if (i >= n && n > 6 && i % n == 4) {
          expansion(i - n) ^ Key.subWord(expansion(i - 1))
        } else {
          expansion(i - n) ^ expansion(i - 1)
        }
      }
    }
    expansion.grouped(4).map(new Key(_)).toSeq
  }

  def bytes: Array[Byte] =
    data.flatMap(BitUtils.wordToUnsignedBytes)
}

object Key {

  private val permittedKeyLength = Set[Int](128, 192, 256).map(_ / 8)
  private val lengthToRounds: Map[Int, Int] =
    Key.permittedKeyLength.zip(Seq(11, 13, 15)).toMap

  def apply(keyData: Array[Byte]): Key = {
    require(permittedKeyLength(keyData.length))

    val data: Array[Int] =
      keyData.grouped(4).map(BitUtils.unsignedBytesToWord).toArray
    new Key(data)
  }

  private val rcon: Vector[Int] =
    Vector(0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1B, 0x36).map(
      x => x << 24
    )

  private[aes] def rotWord(word: Int): Int = {
    val b0 = word & 0xFF
    (word >>> 8) | (b0 << 24)
  }

  private def subWord(word: Int): Int =
    BitUtils.unsignedBytesToWord(
      BitUtils.wordToUnsignedBytes(word).map(SBox.subByte)
    )

}
