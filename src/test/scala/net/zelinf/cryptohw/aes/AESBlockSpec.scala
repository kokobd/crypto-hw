package net.zelinf.cryptohw.aes

import net.zelinf.cryptohw.UnitSpec
import TestUtils._

class AESBlockSpec extends UnitSpec {

  describe("encryptBlock") {
    it("is an inversion of decryptBlock") {
      val keyData = Array(-67, -71, -122, -38, -101, -5, -23, 121, -94, -73,
        -63, 98, -29, 49, 33, 92).map(_.toByte)
      val clearText = Array(37, -85, -38, -107, 37, -22, -24, 54, -125, -49, 64,
        43, -121, 27, 96, -34).map(_.toByte)
      val key = Key(keyData)
      val decryptedText =
        AESBlock.decryptBlock(key, AESBlock.encryptBlock(key, clearText))
      assert(decryptedText.toList == clearText.toList)
    }

    it("works for a known sample") {
      val key = Key("Thats my Kung Fu".getBytes)
      val plainText = "Two One Nine Two".getBytes
      val expected = intsToBytes(0x29, 0xC3, 0x50, 0x5F, 0x57, 0x14, 0x20, 0xF6,
        0x40, 0x22, 0x99, 0xB3, 0x1A, 0x02, 0xD7, 0x3A)
      val actual = AESBlock.encryptBlock(key, plainText)
      assert(actual.toList == expected.toList)
    }
  }
}
