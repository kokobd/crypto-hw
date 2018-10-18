package net.zelinf.cryptohw.aes

import com.oracle.xmlns.internal.webservices.jaxws_databinding.SoapBindingUse
import net.zelinf.cryptohw.math._
import net.zelinf.cryptohw.math.FieldSyntax._

private[aes] final class State(bytes: Seq[Byte]) extends Cloneable {
  require(bytes.length == 128 / 8)

  private val _data: Array[Byte] = bytes.toArray

  def data: Array[Byte] = _data.clone()

  def subBytes(): State = {
    _data.map(SBox.subByte).copyToArray(_data)
    this
  }

  def invSubBytes(): State = {
    _data.map(SBox.invSubByte).copyToArray(_data)
    this
  }

  /**
    *ShiftRows:
    *0 4 8 c ==> 0 4 8 c
    *1 5 9 d     5 9 d 1
    *2 6 a e     a e 2 6
    *3 7 b f     f 3 7 b
    *
    * @return the shifted state
    */
  def shiftRows(): State = {
    val prevData = _data.clone()
    List(0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11).zipWithIndex
      .foreach { case (from, to) => _data.update(to, prevData(from)) }
    this
  }

  /**
    *InvShiftRows:
    *0 4 8 c ==> 0 4 8 c
    *1 5 9 d     d 1 5 9
    *2 6 a e     a e 2 6
    *3 7 b f     7 b f 3
    *
    * @return the shifted state
    */
  def invShiftRows(): State = {
    val indexMapping =
      List(0, 0xD, 0xA, 7, 4, 1, 0xE, 0xB, 8, 5, 2, 0xF, 0xC, 9, 6, 3)
    val prevData = data
    var i = 0
    while (i < indexMapping.size) {
      _data(i) = prevData(indexMapping(i))
      i += 1
    }
    this
  }

  def mixColumns(): State =
    mixColumnsBase(State.mixColumn)

  def invMixColumns(): State =
    mixColumnsBase(State.invMixColumn)

  private def mixColumnsBase(mixer: Array[Byte] => Array[Byte]): State = {
    _data.grouped(4).flatMap(xs => mixer(xs)).copyToArray(_data)
    this
  }

  def addRoundKey(roundKey: Key): State = {
    addAnother(roundKey.bytes)
  }

  def addAnother(that: Array[Byte]): State = {
    _data
      .zip(that)
      .map { case (x, y) => BitUtils.bitwiseXor(x, y) }
      .copyToArray(_data)
    this
  }

  override def equals(other: Any): Boolean = other match {
    case that: State =>
      _data sameElements that._data
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(_data)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def clone(): State = {
    val that = super.clone().asInstanceOf[State]
    _data.copyToArray(that._data)
    that
  }
}

private[aes] object State {

  private[aes] def mixColumn(column: Array[Byte]): Array[Byte] =
    mixColumnBase(column, Array(2, 1, 1, 3))

  private[aes] def invMixColumn(column: Array[Byte]): Array[Byte] =
    mixColumnBase(column, Array(14, 9, 13, 11))

  private def mixColumnBase(column: Array[Byte],
                            coef: Array[Byte]): Array[Byte] = {
    val ts: Array[GF2Elem] =
      column.map(b => GF2Elem(b))
    val us = new Array[GF2Elem](4)
    val as = coef.map(b => GF2Elem(b))

    us(0) = (as(0) |*| ts(0)) |+| (as(3) |*| ts(1)) |+|
      (as(2) |*| ts(2)) |+| (as(1) |*| ts(3))
    us(1) = (as(1) |*| ts(0)) |+| (as(0) |*| ts(1)) |+|
      (as(3) |*| ts(2)) |+| (as(2) |*| ts(3))
    us(2) = (as(2) |*| ts(0)) |+| (as(1) |*| ts(1)) |+|
      (as(0) |*| ts(2)) |+| (as(3) |*| ts(3))
    us(3) = (as(3) |*| ts(0)) |+| (as(2) |*| ts(1)) |+|
      (as(1) |*| ts(2)) |+| (as(0) |*| ts(3))

    val newColumn = column.clone()
    for (i <- 0 to 3)
      newColumn(i) = us(i).value.toByte
    newColumn
  }
}
