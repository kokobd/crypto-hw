package net.zelinf.cryptohw.math
import net.zelinf.cryptohw.UnitSpec

import cats._

final class GF2Test extends UnitSpec {

  describe("GF2Elem(16337060)") {
    it("has a specific inverse") {
      val expected = GF2Elem(BigInt("85915463315859154671793828039326524749"))
      val rp = BigInt("111", 2) ^ (BigInt(1) << 13) ^ (BigInt(1) << 131)
      implicit val rpIns: Field[GF2Elem] = GF2Elem.newGF2ElemFieldInstance(rp)

      assert(Eq.eqv(expected, Field.mulInvert(GF2Elem(BigInt(16337060)))))
    }
  }
}
