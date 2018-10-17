package net.zelinf.cryptohw.aes

import net.zelinf.cryptohw.math._
import net.zelinf.cryptohw.math.FieldSyntax._

private[aes] final class State(bytes: Seq[Byte]) {
  require(bytes.length == 128 / 8)

  private val _data: Array[Byte] = bytes.toArray

  def data: Array[Byte] = _data.clone()

  def subBytes(): State = {
    _data.map(SBox.subByte)
    this
  }

  def invSubBytes(): State = {
    ???
  }

  def shiftRows(): State = {
    val prevData = _data.clone()
    List(0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11).zipWithIndex
      .foreach { case (from, to) => _data.update(to, prevData(from)) }
    this
  }

  def invShiftRows(): State = {
    ???
  }

  def mixColumns(): State = {
    def mixColumn(col: Int): State = {
      val theColumn = _data.view(4 * col, 4 * col + 4)

      val ts: Array[GF2Elem] =
        theColumn.toArray.map(b => GF2Elem(b))
      val us = new Array[GF2Elem](4)
      val x = GF2Elem(2)
      val y = GF2Elem(3)

      us(0) = (x |*| ts(0)) |+| (y |*| ts(1)) |+| ts(2) |+| ts(3)
      us(1) = (x |*| ts(1)) |+| (y |*| ts(2)) |+| ts(3) |+| ts(0)
      us(2) = (x |*| ts(2)) |+| (y |*| ts(3)) |+| ts(0) |+| ts(1)
      us(3) = (x |*| ts(3)) |+| (y |*| ts(0)) |+| ts(1) |+| ts(2)

      for (i <- 0 to 3)
        theColumn(i) = us(i).value.toByte
      this
    }

    for (col <- 0 to 3)
      mixColumn(col)
    this
  }

  def invMixColumns(): State = {
    ???
  }

  def addRoundKey(roundKey: Key): State = {
    roundKey.bytes.zip(_data).map { case (k, s) => k ^ s }
    this
  }
}
