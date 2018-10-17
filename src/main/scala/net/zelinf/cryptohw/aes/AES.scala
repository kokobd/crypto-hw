package net.zelinf.cryptohw.aes

object AES {

  private[aes] def encryptBlock(key: Key,
                                plainText: Array[Byte]): Array[Byte] = {
    val state = new State(plainText)
    val roundKeyIt = key.roundKeys.iterator

    state.addRoundKey(roundKeyIt.next())
    while (roundKeyIt.hasNext) {
      state.subBytes().shiftRows()
      if (roundKeyIt.size > 1)
        state.mixColumns()
      state.addRoundKey(roundKeyIt.next())
    }

    state.data
  }
}
