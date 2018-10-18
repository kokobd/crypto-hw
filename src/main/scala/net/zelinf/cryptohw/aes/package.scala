package net.zelinf.cryptohw

import net.zelinf.cryptohw.math.{BitUtils, Field, GF2Elem}
import collection.mutable

package object aes {

  private[aes] implicit val fieldIns: Field[GF2Elem] =
    GF2Elem.newGF2ElemFieldInstance(BigInt("100011011", 2))

  def encryptCBC(iv: Array[Byte],
                 key: Key,
                 plainText: Array[Byte]): Array[Byte] = {
    val BLOCK_SIZE = 128 / 8
    require(iv.length == BLOCK_SIZE)
    val plainText_ = padPkc5(plainText, 128 / 8)
    val plainBlocks = plainText_.grouped(128 / 8).toVector
    val cipherBlocks = mutable.ArrayBuffer[Array[Byte]]()
    for (i <- plainBlocks.indices) {
      val mixed =
        xorAll(plainBlocks(i), if (i == 0) iv else cipherBlocks(i - 1))
      cipherBlocks += AESBlock.encryptBlock(key, mixed)
    }
    cipherBlocks.toArray.flatten
  }

  private def xorAll(xs: Array[Byte], ys: Array[Byte]): Array[Byte] = {
    xs.zip(ys).map { case (x, y) => BitUtils.bitwiseXor(x, y) }
  }

  private[aes] def padPkc5(data: Array[Byte], blockSize: Int): Array[Byte] = {
    val n: Int = blockSize - data.length % blockSize
    val padding = Array.fill[Byte](n)(n.toByte)
    data ++ padding
  }

  private[aes] def showUBytes(bytes: Seq[Byte]): String = {
    bytes
      .map(b => BitUtils.unsignedByteToWord(b).toHexString)
      .mkString("[", ", ", "]")
  }
}
