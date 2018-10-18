package net.zelinf.cryptohw.aes

private[aes] object AESBlock {

  private[aes] def encryptBlock(key: Key,
                                plainText: Array[Byte]): Array[Byte] = {
    val state = new State(plainText.clone())
    val roundKeyIt = key.roundKeys.iterator

    state.addRoundKey(roundKeyIt.next())
    while (roundKeyIt.hasNext) {
      val roundKey = roundKeyIt.next()
      state.subBytes().shiftRows()
      if (roundKeyIt.hasNext)
        state.mixColumns()

      state.addRoundKey(roundKey)
    }

    state.data
  }

  private[aes] def decryptBlock(key: Key,
                                cipherText: Array[Byte]): Array[Byte] = {
    val state = new State(cipherText.clone())
    val roundKeyIt = key.roundKeys.reverseIterator

    state.addRoundKey(roundKeyIt.next())
    var isFirst = true
    while (roundKeyIt.hasNext) {
      if (!isFirst)
        state.invMixColumns()
      else {
        isFirst = false
      }
      state.invShiftRows().invSubBytes()
      state.addRoundKey(roundKeyIt.next())
    }

    state.data
  }
}
