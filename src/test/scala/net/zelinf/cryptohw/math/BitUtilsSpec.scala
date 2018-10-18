package net.zelinf.cryptohw.math

import net.zelinf.cryptohw.UnitSpec
import BitUtils._

final class BitUtilsSpec extends UnitSpec {

  describe("unsignedBytesToWord") {
    it("combines bytes") {
      val actual =
        unsignedBytesToWord(Array(0xAA, 0xBB, 0xCC, 0xDD).map(_.toByte))
      assert(actual == 0xDDCCBBAA)
    }
  }
}
