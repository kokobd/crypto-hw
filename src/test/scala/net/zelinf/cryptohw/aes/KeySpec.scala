package net.zelinf.cryptohw.aes
import net.zelinf.cryptohw.UnitSpec

final class KeySpec extends UnitSpec {

  describe("rotWord") {
    it("works for a sample") {
      assert(Key.rotWord(0xAABBCCDD) == 0xDDAABBCC)
    }
  }
}
