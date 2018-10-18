package net.zelinf.cryptohw.aes
import net.zelinf.cryptohw.UnitSpec

class StateSpec extends UnitSpec {

  describe("shiftRows") {
    it("shifts an example as expected") {
      val s = new State((0 to 0xF).map(_.toByte))
      val expected = new State(
        Array(0, 5, 0xA, 0xF, 4, 9, 0xE, 3, 8, 0xD, 2, 7, 0xC, 1, 6,
          0xB).map(_.toByte)
      )
      assert(s.shiftRows() == expected)
    }
  }

  describe("invShiftRows") {
    it("is an inversion of shiftRows") {
      val s = new State((0 to 0xF).map(_.toByte))
      val s_copy = s.clone()
      assert(s.shiftRows().invShiftRows() == s_copy)
    }
  }

  describe("mixColumns") {
    it("works for some samples") {
      val s = new State(
        Array(0xDB, 0x13, 0x53, 0x45, 0xF2, 0x0A, 0x22, 0x5C, 0x01, 0x01, 0x01,
          0x01, 0xD4, 0xD4, 0xD4, 0xD5).map(_.toByte)
      )
      s.mixColumns()
      val expected = new State(
        Array(0x8E, 0x4D, 0xA1, 0xBC, 0x9F, 0xDC, 0x58, 0x9D, 0x01, 0x01, 0x01,
          0x01, 0xD5, 0xD5, 0xD7, 0xD6).map(_.toByte)
      )
      assert(s == expected)
    }
  }

  describe("invMixColumns") {
    it("is an inversion of mixColumns") {
      val s = new State(
        Array(0xDB, 0x13, 0x53, 0x45, 0xF2, 0x0A, 0x22, 0x5C, 0x01, 0x01, 0x01,
          0x01, 0xD4, 0xD4, 0xD4, 0xD5).map(_.toByte)
      )
      val s_copy = s.clone()
      s.mixColumns()
      s.invMixColumns()
      assert(s == s_copy)
    }
  }
}
