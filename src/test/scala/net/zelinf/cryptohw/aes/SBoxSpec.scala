package net.zelinf.cryptohw.aes

import net.zelinf.cryptohw.UnitSpec
import SBox._

class SBoxSpec extends UnitSpec {

  describe("subByte") {
    it("works for some known results") {
      assert(subByte(0x00) == 0x63.toByte)
      assert(subByte(0x53) == 0xED.toByte)
      assert(subByte(0xEC.toByte) == 0xCE.toByte)
    }
  }
}
