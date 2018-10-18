package net.zelinf.cryptohw.aes

object TestUtils {
  private[aes] def intsToKey(bytes: Int*): Key =
    Key(bytes.map(_.toByte).toArray)

  private[aes] def intsToBytes(ints: Int*): Array[Byte] =
    ints.map(_.toByte).toArray
}
